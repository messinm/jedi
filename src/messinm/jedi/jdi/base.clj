
;
;   Copyright (c) April 2009 Mike Messinides. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;
;   You must not remove this notice, or any other, from this software.
;
;Note: requires tool.jar from the jdk
;
(ns messinm.jedi.jdi.base

(import [java.io BufferedInputStream BufferedWriter OutputStreamWriter]))


(defn con-args
[con main opts]
(let [da       (. con (defaultArguments))
      damap    (zipmap (keys da) (vals da))
      opt-arg  (damap "options")
      main-arg (damap "main")]
   (assoc damap "options" (doto opt-arg (.setValue opts))
               "main" (doto main-arg (.setValue main)))))

(defn conarg-help
[conarg]
(doseq  [e conarg] (do (println (key e) (val e))
                       (println (. (val e) (description))))))


(defn pstreams
"start threads to monitor the error and out streams of a Process, which print the characters read.
Returns a function which takes a string and writes it to the input stream of the Process."
[proc] 
(let [out-strm (new java.io.BufferedInputStream (. proc (getInputStream)))
      err-strm (new java.io.BufferedInputStream (. proc (getErrorStream)))
      in-wrt   (new java.io.BufferedWriter (new java.io.OutputStreamWriter (. proc (getOutputStream))))
      strm-fn   (fn strm-fn [strm] (loop [c (. strm (read))]
                                      (when (not= c -1) (do (print (char c))                                                           
                                                    (recur (. strm (read)))))))
      out-thr (new Thread (fn [] (strm-fn out-strm)))
      err-thr (new Thread (fn [] (strm-fn err-strm)))
      wrt-fn  (fn wrt-fn [#^String txt] (let [n (count txt)]
                                           (. in-wrt (write txt 0 n))
                                           (. in-wrt (newLine))
                                           (. in-wrt (flush))))]
    (. out-thr (start))
    (. err-thr (start))
    [wrt-fn out-thr err-thr]))

(defn vmq->sq
"retrieves events from the debug vm event queue, and puts each event into a SynchronousQueue. Returns 
[kill-fn sq], where kill-fn is a thunk to stop the queue service thread, and sq is the SynchronousQueue for 
retrieving events. prints each event if pflag is non-nil."
([virtm pflag] (let [eq      (. virtm (eventQueue))
                     runflag (ref true)
                     sq      (new java.util.concurrent.SynchronousQueue)
                     kill    (fn[] (dosync (ref-set runflag false)))        
                     q-fn    (fn q-fn [] (loop [e (. eq (remove))]
                               (when pflag (println e))
                               (.put sq e)
                               (when @runflag (recur (. eq (remove))))))
                     q-thr  (new Thread q-fn)]
      (. q-thr (start))
      [kill sq]))
([virtm] (vmq->sq virtm true)))

(defn get-event
[synq] (. synq (poll (long 2) java.util.concurrent.TimeUnit/SECONDS)))

(defn proc-events
"Start a thread to retrieve events from the debug vm event queue, and calls function callback with the event set.
Returns a thunk which stops the thread when called"
[virtm callback]
(let [eq        (.eventQueue virtm)
      rem-evset (fn rem-ev [] (try (.remove eq (long 1000))
                                 (catch Exception e e)))
      runflag (ref true)
      kill-fn (fn kill-fn [] (dosync (ref-set runflag false)))
      proc-fn (fn proc-fn [] (loop [e (rem-evset)]
                                (when (and @runflag e) 
                                   (callback e))
                                (when @runflag
                                    (recur (rem-evset)))))
      thr      (new Thread proc-fn)]
   (.start thr)
   kill-fn))


(defn peek-threads
[virtm]
(do
(. virtm (suspend))
(doseq [t (. virtm (allThreads))] (try (println t) (doseq [f (. t (frames))] (println f)) 
                                  (catch Exception e (println e))))
(. virtm (resume))))



(defn fn-munge
"returns the internal representation of a function name, used for naming classes created by the clojure compiler" 
[name] (let [mname (. clojure.lang.Compiler (munge name))]
          (.replace mname ".", "_DOT_")))



(defn get-meth-descr
"Returns a seq of seqs with descriptions of methods in the debug vm virtm. Supplied methods are as obtained from the get-meths function.
The description of each method is a string 'name [arg0 arg1 ...]'" 
[cls-meths]  
            (map (fn [meths]
             (map  (fn [m]
               (let [meth-name (.name m)
                     args      (try (.arguments m) (catch Exception e))]
                 (when args (let [
                     argvec    (into [] (for [a args] (.name a)))
                     argstr0   (reduce #(str %1 " " %2) "[" (butlast argvec))
                     argstr    (str argstr0 (when (= meth-name "doInvoke") " &") " " (last argvec) "]")] 
                  (print-str (.name (.declaringType m)) argstr))))) meths)) cls-meths))

(defn all-methods
[cls] (try (.allMethods cls)
        (catch Exception e '())))


(defn get-meths
"Using the debug vm specified by virtm, obtains methods used to implement clojure function(s) named by name. Returns a seq of seqs, 
one outer seq for each class that matches name. The inner seq has class methods each corresponding to a clojure function clause."
[virtm name] (let [mname     (fn-munge name)          
                   cls-meths (map (fn[cls] (filter #(and (. (.name (.declaringType %)) (contains mname))
                                ;doInvoke => rest arg
                            (or (. (.name %) (contains "doInvoke"))
                                 (. (.name %) (contains "invoke")))) (all-methods cls))) 
                                    (filter #(. (.name %) (contains mname)) (. virtm (allClasses))))
                   meth-desc  (get-meth-descr cls-meths)]
              (when cls-meths (with-meta cls-meths {:meth-desc meth-desc}))))



;!!!!!!!!!!!!!!!!!!

(defn get-clj-descr
"Returns a seq of seqs with descriptions of methods in the debug vm virtm. Supplied methods are as obtained from the get-meths function.
The description of each method is a string 'name [arg0 arg1 ...]'" 
[cls-meths]  
            (map (fn [meths]
             (map  (fn [m]
               (let [meth-name (.name m)
                     args      (try (.arguments m) (catch Exception e))]
                 (when args (let [
                     argvec    (into [] (for [a args] (.name a)))
                     argstr0   (reduce #(str %1 " " %2) "[" (butlast argvec))
                     argstr    (str argstr0 (when (= meth-name "doInvoke") " &") " " (last argvec) "]")] 
                  (print-str (.name (.declaringType m)) argstr))))) meths)) cls-meths))

(defn get-jav-descr
[cls-meths] 
(map (fn [meths] (map #(.toString %) meths)) cls-meths))


(defn get-meths2
"Using the debug vm specified by virtm, obtains methods used to implement clojure function(s) named by name. Returns a seq of seqs, 
one outer seq for each class that matches name. The inner seq has class methods each corresponding to a clojure function clause."
[virtm name] (let [mname     (fn-munge name)  
                   all-cl    (.allClasses virtm)
                   clj-meths (map (fn[cls] (filter #(and (. (.name (.declaringType %)) (contains mname))
                                ;doInvoke => rest arg
                            (or (. (.name %) (contains "doInvoke"))
                                 (. (.name %) (contains "invoke")))) (all-methods cls))) 
                                    (filter #(. (.name %) (contains mname)) (. virtm (allClasses))))
                   jav-meths  (filter #(> (count %) 0) (map (fn [meths] (filter #(try (. (.name %) (contains name)) (catch Exception e nil)) meths)) 
                                     (map #(try (.methods %) (catch Exception e '())) all-cl)))
                   clj-desc   (get-clj-descr clj-meths)
                   jav-desc   (get-jav-descr jav-meths)]
              (with-meta (concat clj-meths jav-meths) {:meth-desc (concat clj-desc jav-desc)})))

;!!!!!!!!!!!!
(defn meths->map 
"takes a seq of class methods from get-meths and returns a map where each key is the string description of the method contained in the
corresponding val"
[cls-meths]
(let [cm (apply concat cls-meths)
      mm (apply concat ((meta cls-meths) :meth-desc))]
  (apply sorted-map (interleave mm cm))))

;(meths->map (get-meths vm "agent"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;breakpoints and stepping;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-bp
"Set and enable a breakpoint at the top of the supplied method"
[virtm meth]
(let [erm  (.eventRequestManager virtm) 
      bp   (.createBreakpointRequest erm (.location meth))]
  (.enable bp)
  bp))


(def step-const {;depth
                  :into com.sun.jdi.request.StepRequest/STEP_INTO
                  :out  com.sun.jdi.request.StepRequest/STEP_OUT
                  :over com.sun.jdi.request.StepRequest/STEP_OVER
                 ;size
                  :line com.sun.jdi.request.StepRequest/STEP_LINE
                  :min  com.sun.jdi.request.StepRequest/STEP_MIN})

(defn thread-by-name
[virtm tname]
(let [ts (.allThreads virtm)]
   (first (filter #(= (.name %) tname) ts))))


(defn step
[virtm thr size depth]
(let [sz  (step-const size)
      dp  (step-const depth)
      erm (.eventRequestManager virtm) 
      stp (.createStepRequest erm thr sz dp)]
   (.addCountFilter stp 1) ;one-time step
   (.enable stp)
   (.resume virtm)
   stp))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;clojure eval and other functions in debugee;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn around-invoke
"disables event requests before executing work-fn, then re-enables after work-fn returns. Returns the return value of work-fn."
[virtm work-fn]
(let [erm         (.eventRequestManager virtm)
      bpts        (.breakpointRequests erm)
      steps       (.stepRequests erm)
      dis-fn      (fn[er] (let [en? (.isEnabled er)]
                                 (.disable er) 
                                  en?))
      en-fn       (fn[er en?] (when en? (.enable er) nil))
      bpt-state   (doall (map dis-fn bpts))
      stp-state   (doall (map dis-fn steps))
      result      (work-fn)
      _           (doall (map en-fn bpts bpt-state))
      _           (doall (map en-fn steps stp-state))]
  result))


(defn mirror-print
"print-to-string the mirror value in the debugee running in virtual machine virtm. Returns the string. Thread thr must have been suspended
by an event specific to the thread (eg a breakpoint). Suspending the vm and using an arbitrary thread will result in an exception.
Caution: if attempting to print an infinite seq, this method will hang."
[virtm thr mirror]
(let [  ;may get multiple classes from multiple classloaders, may need to fix later
      rt-cls        (first (.classesByName virtm "clojure.lang.RT"))
      print-str     (.concreteMethodByName rt-cls "printString" "(Ljava/lang/Object;)Ljava/lang/String;")
      mirror-str    (.invokeMethod rt-cls thr print-str (list mirror) com.sun.jdi.ObjectReference/INVOKE_SINGLE_THREADED)]
   (.value mirror-str)))

(defn mirror-eval
"evaluate the string fstr in the debugee running in virtual machine virtm. Thread thr must have been suspended
by an event specific to the thread (eg a breakpoint). Suspending the vm and using an arbitrary thread will result in an exception.
When single-thr? is non-nil, the debugee method invocations will be done using the single-threaded flag - see JPDA docs for info"
([virtm thr fstr] (mirror-eval virtm thr fstr true))
([virtm thr fstr single-thr?]
(let [  ;may get multiple classes from multiple classloaders, may need to fix later
      flag          (if single-thr? com.sun.jdi.ObjectReference/INVOKE_SINGLE_THREADED 0)
      rt-cls        (first (.classesByName virtm "clojure.lang.RT"))
      cmp-cls       (first (.classesByName virtm "clojure.lang.Compiler"))
      read-str      (.concreteMethodByName rt-cls "readString" "(Ljava/lang/String;)Ljava/lang/Object;")
      dbg-eval        (.concreteMethodByName cmp-cls "eval" "(Ljava/lang/Object;)Ljava/lang/Object;")
      form-str          (.mirrorOf virtm fstr)
      form-mirror      (.invokeMethod rt-cls thr read-str (list form-str) flag)]
    (.invokeMethod cmp-cls thr dbg-eval  (list form-mirror) flag) )))





(defn mirror-def
"def a var in the debugee with the supplied mirror value as root. nsname and varname are strings for interning the new var in a namespace.
When single-thr? is non-nil, the debugee method invocations will be done using the single-threaded flag - see JPDA docs for info"
([virtm thr mirror #^String nsname #^String varname] (mirror-def virtm thr mirror nsname varname true))
([virtm thr mirror #^String nsname #^String varname single-thr?]
(let [  ;may get multiple classes from multiple classloaders, may need to fix later
      flag          (if single-thr? com.sun.jdi.ObjectReference/INVOKE_SINGLE_THREADED 0)
      ns-cls        (first (.classesByName virtm "clojure.lang.Namespace"))
      var-cls       (first (.classesByName virtm "clojure.lang.Var"))
      sym-cls       (first (.classesByName virtm "clojure.lang.Symbol"))
      sym-intern    (.concreteMethodByName sym-cls "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;") ;intern (not create) to avoid dupe namespaces
      var-bind      (.concreteMethodByName var-cls "bindRoot" "(Ljava/lang/Object;)V")
      ns-findCreate (.concreteMethodByName ns-cls "findOrCreate" "(Lclojure/lang/Symbol;)Lclojure/lang/Namespace;")
      ns-intern      (.concreteMethodByName ns-cls "intern" "(Lclojure/lang/Symbol;)Lclojure/lang/Var;")

      ns-sym       (.invokeMethod sym-cls thr sym-intern (list (.mirrorOf virtm nsname)) flag)
      namsp        (.invokeMethod ns-cls thr ns-findCreate (list ns-sym) flag)
      var-sym      (.invokeMethod sym-cls thr sym-intern (list (.mirrorOf virtm varname)) flag)
      newvar       (.invokeMethod namsp thr ns-intern (list var-sym) flag)]
   (.invokeMethod newvar thr var-bind (list mirror) flag))))

;;demo
(comment

(def vmm (.  com.sun.jdi.Bootstrap (virtualMachineManager)))
(def defcon (. vmm (defaultConnector)))
(def vm (. defcon (launch (con-args defcon "clojure.lang.Repl" "-cp clojure.jar;./classes;./lib/*"))))
(def vmq-vec (vmq->sq vm))
(def vm-proc (. vm (process)))
(def proc-thrs (pstreams2 vm-proc))
(def erm (.eventRequestManager vm)) 

(. vm (resume))
(def all-classes (. vm (allClasses)))
;(map #(.name %) all-classes)
(def clj-core-classes (filter #(. (.name %) (contains "clojure.core")) all-classes))

(map #(.name %) (.arguments (ffirst (get-meths vm "+"))))

(map #(.name %) (.arguments (ffirst (get-meths vm "ancestors"))))
(map #(.name %) (.variables (ffirst (get-meths vm "ancestors"))))

;breakpoint

(def bpt+ (.createBreakpointRequest erm (.location (second (first (get-meths vm "+"))))))
(.enable bpt+)
;debug REPL "(+ 1 2)" ; no break, because + is inlined into another class
;debug REPL "(apply + (list 1 2))" ; => breakpoint

(def bpe (get-event sq)) ; get the breakpoint event set

(.visibleVariables (.frame (.thread #^com.sun.jdi.event.LocatableEvent (first (.toArray bpe))) 0))
(into [] (.getArgumentValues (.frame (.thread #^com.sun.jdi.event.LocatableEvent (first (.toArray bpe))) 0)))

(let [thr (.thread #^com.sun.jdi.event.LocatableEvent (first (.toArray bpe)))]
(doseq [vmirror (.getArgumentValues (.frame thr 0))]
    (let [cls   (.referenceType vmirror)
          toStr (.concreteMethodByName cls "toString" "()Ljava/lang/String;")
          val   (.invokeMethod vmirror thr toStr (new java.util.ArrayList) com.sun.jdi.ObjectReference/INVOKE_SINGLE_THREADED) ]
     (println vmirror cls (.value val)))))

(let [thr   (.thread #^com.sun.jdi.event.LocatableEvent (first (.toArray bpe)))
      numfr (.frameCount thr)]
  (println numfr "frames")
  (doseq [i (range numfr)] (println (get-thr-vars thr i))))

(let [thr   (.thread #^com.sun.jdi.event.LocatableEvent (first (.toArray bpe)))]
 (try
  (mirror-val (mirror-eval vm thr "(+ 1 1)"))
  (catch com.sun.jdi.InvocationException e (mirror-val (.exception e)))))


(.suspend vm)

(mirror-tree (with-meta (seq (.allThreads vm)) {:tag :threads}) "threads")
;(.resume vm)

;(.exit vm 0)
)

(comment

(in-ns 'jdi)
(def vmm (.  com.sun.jdi.Bootstrap (virtualMachineManager)))
(def defcon (. vmm (defaultConnector)))
(def vm (. defcon (launch (con-args defcon "clojure.lang.Repl" "-cp clojure.jar;./classes;./lib/*"))))
(def vmq-vec (vmq->sq vm))
(def vm-proc (. vm (process)))
(def proc-thrs (pstreams2 vm-proc))
(def erm (.eventRequestManager vm))
(. vm (resume))
(get-event (second vmq-vec)) ;start-up event

(def bpt+ (.createBreakpointRequest erm (.location (second (first (get-meths vm "+"))))))
(.enable bpt+)
;debug REPL "(+ 1 2)" ; no break, because + is inlined into another class
;debug REPL "(apply + (list 1 2))" ; => breakpoint

(def bpe (get-event (second vmq-vec))); get the breakpoint event set

(step vm (thread-by-name vm "main") :line :into)
(def step-0 (get-event (second vmq-vec)))
(mirror-tree (with-meta (seq (.allThreads vm)) {:tag :threads}) "threads")
;delete previous step request
(let [req (first (.stepRequests erm))]
  (.deleteEventRequest erm req))
(step vm (thread-by-name vm "main") :line :out)
(def step-1 (get-event (second vmq-vec)))
;close previous threads tree, then:
(mirror-tree (with-meta (seq (.allThreads vm)) {:tag :threads}) "threads")

)
