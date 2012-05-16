
;
;   Copyright (c) April 2009 Mike Messinides. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;
;   Contains a derivative of Rich Hickey's inspector tree from the Clojure libraries, Copyright (c) Rich Hickey
;
;   You must not remove this notice, or any other, from this software.
;
(ns messinm.jedi.jdi.gui
  
  (import [javax.swing JTextArea JSplitPane JFrame JScrollPane JTextField JLabel JPopupMenu JMenuItem JOptionPane JComboBox JList
                      BorderFactory]
          [javax.swing.event ListSelectionEvent ListSelectionListener]
          [java.awt Dimension] 
          [java.awt.event KeyListener WindowListener MouseAdapter MouseEvent ActionListener])

  (use  [messinm.jedi.swing utils]
        [messinm.jedi.jdi base]))

;;;;;;;;;;;print methods - needed for breakpoint listing in gui 

(defmethod print-method com.sun.jdi.request.BreakpointRequest
[#^com.sun.jdi.request.BreakpointRequest bpt #^java.io.Writer w] 
(print-method (print-str (if (.isEnabled bpt) "[ENABLED]" "[disabled]") "BreakpointRequest at " (.toString (.location bpt))) w))

(defmethod print-method :bpt-info
[bpt-info #^java.io.Writer w] 
(let [bpt (bpt-info :bpt)
      thr (bpt-info :thr)]
(binding [*print-readably* false]
 (print-method bpt w)
 (when thr (print-method (print-str "[thread: " (.name thr) "]") w)))))

;;;;;;;;;;;;;;

;appstate - a map used to hold objects for interaction between debugger application components, and to allow REPL interaction with the 
;debugger and debugged process
;
;  :vm -> debugee VirtualMachine object
;  :pre-eval -> no-arg function 
;                        called just prior to debugee eval (to blank thread tree due to thread being resumed, frames are then invalid)
;  :post-eval -> no-arg function 
;                        called just after debugee eval (to repopulate thread tree)
;  :exit-fn -> no arg function
;                         called to exit debugging
;  :debug-frame -> JFrame with debugging components
;  :proc-frame -> JFrame with stdin, stdout, and stderr for debugee
;  :mir-def-exc -> exception from def'ing in debugee
;  :eval-exc  -> exception from eval in debugee 
;  :bpt-upd -> no-arg fn to update breakpoint listing (note: run in Swing thread)
;  :step-fn -> fn for step after a breakpoint, args [size depth _] 
;                  size and depth are keywords, see jdi/base.clj step-const map for options, _ is currently ignored
;  :sel-bpt -> currently selected (in gui list) breakpoint
;  :cur-event -> most recent debugee event set
;  :filt-dlg-results -> vector [ok? value] from filter dialog
;  :def-dlg-results -> vector [ok? ns-txt var-txt] from "def in debugee" dialog
;  :bpt-info -> a map containing breakpoint information, see 
;
(def appstate (ref {}))




(defn make-proc-frame
"constructs a swing gui for process interaction. title is the JFrame title, and wrt is the Writer used to send characters to the process.
Returns [out err] where out and err are functions, each of which accepts a character for writing to the gui output and error displays."
[title wrt]
(let [
      frame   (new javax.swing.JFrame title)
      out-txt  (doto (new JTextArea) (.setEditable false))
      err-txt  (doto (new JTextArea) (.setEditable false))
      append-out (fn appender [s] (.append out-txt s))
      append-err (fn appender [s] (.append err-txt s))
      in-area (new JTextArea) 
      in-doc  (.getDocument in-area)
      elem    (. in-doc (getDefaultRootElement))      
      outpane (sw-lbl-txtarea out-txt "output stream")
      errpane (sw-lbl-txtarea err-txt "error stream")
      splpane-0 (sw-split :v outpane errpane)      
      inpane  (sw-lbl-txtarea in-area "input")
      splpane-1 (sw-split :v  inpane splpane-0)
      lstnr   (proxy [KeyListener] []
               (keyPressed [evt] )
               (keyReleased [evt] )
               (keyTyped [evt] (let [ch (. evt (getKeyChar))]
                                (when (not= ch java.awt.event.KeyEvent/CHAR_UNDEFINED)  (.write wrt (int ch))
                                     (.flush wrt)))))]
      (.addKeyListener in-area lstnr)        
      (doto frame 
                 (.add splpane-1) 
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)		
                (.setSize 700 700)
                (.setVisible true))
      [append-out append-err frame]))

(defn proc-gui
"Creates a gui and threads to service the error and out streams of a Process. The gui also has an input text area
which sends characters to the input stream of the process"
[title proc app-state] 
(let [out-strm            (new java.io.BufferedInputStream (. proc (getInputStream)))
      err-strm            (new java.io.BufferedInputStream (. proc (getErrorStream)))
      in-wrt              (new java.io.BufferedWriter (new java.io.OutputStreamWriter (. proc (getOutputStream))))
      [app-out app-err proc-frame]   (make-proc-frame title in-wrt)
      strm-fn   (fn strm-fn [strm apfn] (loop [c (. strm (read))]
                                      (when (not= c -1) (do (apfn (str (char c)))                                                           
                                                    (recur (. strm (read)))))))
      out-thr (doto (new Thread (fn [] (strm-fn out-strm app-out))) (.setDaemon true))
      err-thr (doto (new Thread (fn [] (strm-fn err-strm app-err))) (.setDaemon true))]
    (. out-thr (start))
    (. err-thr (start))
    (dosync (commute app-state assoc :proc-frame proc-frame :out-thr out-thr :err-thr err-thr))
  proc-frame))

;;;;;;;;;;;;;;;JTree for displaying threads,frames,locals, and fields;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;values to text;;;;;;;;;;;

(defn simple?
[vmirror] 
(let [cls     (class vmirror)]
   (or (isa? cls com.sun.jdi.PrimitiveValue) (isa? cls com.sun.jdi.StringReference))))

(defn cljdata?
[obj] (and (not (instance? clojure.lang.MapEntry obj )) (or (seq? obj) (associative? obj) )))

(defn mfn-selector
[parent] 
(let [r  (cond 
             (instance? clojure.lang.IMapEntry parent)        clojure.lang.IMapEntry
              (cljdata? parent)                              :cljdata
              (simple? parent)                               :simple
              :default                                       (class parent))]
  ;(println "[mfn-selector]" parent r)
   r))

(declare is-leaf)

(defmulti mirror-txt mfn-selector)

(defmethod mirror-txt com.sun.jdi.ObjectReference
[vmirror] 
(let  [cls   (.referenceType vmirror)
      hcode  (.hashCode vmirror)]
 ;(println "mirror-txt objref" vmirror) 
  (str "instance of " cls "@" (Integer/toHexString hcode))))


(defmethod mirror-txt com.sun.jdi.ArrayReference
[vmirror] 
(let  [cls   (.referenceType vmirror)
      hcode  (.hashCode vmirror)]
 ;(println "mirror-txt arrref" vmirror) 
  (str "[array]" cls "@" (Integer/toHexString hcode))))

(defmethod mirror-txt com.sun.jdi.ThreadReference
[vmirror] (do ;(println "mirror-txt thread" vmirror) 
(.name vmirror)))

(defmethod mirror-txt com.sun.jdi.StackFrame
[vmirror] (do ;(println "mirror-txt frame" vmirror) 
(str "[Frame]" (mirror-txt (.location vmirror)))))

(defmethod mirror-txt com.sun.jdi.Location
[vmirror] (do ;(println "mirror-txt loc" vmirror) 
(.toString vmirror)))

(defmethod mirror-txt com.sun.jdi.LocalVariable
[vmirror] (do ;(println "mirror-txt locvar" vmirror) 
(.toString vmirror)))

(defmethod mirror-txt com.sun.jdi.Field
[vmirror] (do ;(println "mirror-txt field" vmirror) 
(str "instance of " (.type vmirror) )))

(defmethod mirror-txt com.sun.jdi.Value
[vmirror] 
(do ;(println "mirror-txt val" vmirror) 
  (str "instance of " (.typeName vmirror) )))

(defmethod mirror-txt :simple
[vmirror] 
(do ;(println "mirror-txt :simple" vmirror)  
  (str (.value vmirror))))

(def cljdata-text {:fields "<Fields>" :locals "<Locals>" :threads "Threads"}) 

(defmethod mirror-txt :cljdata
[cljmap] (do ;(println "mirror-txt :cljdata" cljmap (class cljmap)) 
             (cljdata-text (:tag (meta cljmap)))))

(defmethod mirror-txt clojure.lang.IMapEntry
[map-entry] 
(let [k            (key map-entry)
      v            (val map-entry)
      vtxt         (cond
                    (nil? v) "nil"
                    (is-leaf v) (mirror-txt v)
                    :default  nil)]
      ;_      (println "mirror-txt  clojure.lang.MapEntry" map-entry) ]
   (str k (when vtxt (str " [" (.typeName k) "] = "  vtxt))))) 

(defmethod mirror-txt :default
[vmirror] (do ;(println "mirror-txt :default" vmirror) 
             (if vmirror (.toString vmirror) "nil")))  

;;;;;;;;;;;;;JTree isLeaf;;;;;;;;;;   

(defmulti is-leaf mfn-selector )

(defmethod is-leaf com.sun.jdi.ObjectReference
[vmirror] false)

(defmethod is-leaf com.sun.jdi.ArrayReference
[vmirror] false)

(defmethod is-leaf com.sun.jdi.ThreadReference
[vmirror] false)

(defmethod is-leaf com.sun.jdi.StackFrame
[vmirror] false)

(defmethod is-leaf com.sun.jdi.Location
[vmirror] true)

(defmethod is-leaf com.sun.jdi.Value
[vmirror] false)

(defmethod is-leaf :simple
[vmirror] true)

(defmethod is-leaf clojure.lang.IMapEntry
[map-entry] 
(is-leaf (val map-entry))) 

(defmethod is-leaf :cljdata
[vmirror] false)

(defmethod is-leaf :default
[vmirror] true)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defmulti get-child-count #(mfn-selector %1))

(defmethod get-child-count com.sun.jdi.ObjectReference
[vmirror] 1) ;return a map with fields


(defmethod get-child-count com.sun.jdi.ArrayReference
[vmirror]
(.length vmirror))

(defmethod get-child-count com.sun.jdi.ThreadReference
[vmirror]
(.frameCount vmirror))

(defmethod get-child-count com.sun.jdi.StackFrame
[vmirror] 1) ;return a map with locals


(defmethod get-child-count :cljdata
[data] (count data))

(defmethod get-child-count com.sun.jdi.Value
[data] 1)

(defmethod get-child-count :simple
[data] 0)

(defmethod get-child-count clojure.lang.IMapEntry
[map-entry] (if (simple? (val map-entry)) 0 1))

(defmethod get-child-count :default
[vmirror] 0)
             ;;;;;;;;;;;;;;;;;;

(defmulti get-child (fn [parent idx] (mfn-selector parent)))

(defmethod get-child com.sun.jdi.ObjectReference
[vmirror idx] 
(let [cls    (.referenceType vmirror)]
  (with-meta (into {} (.getValues vmirror (.allFields cls))) {:tag :fields})))

(defmethod get-child com.sun.jdi.ArrayReference
[vmirror idx] (or (.getValue vmirror idx) "nil"))
                

(defmethod get-child :cljdata
[hmap idx] (nth (seq hmap) idx))

(defmethod get-child clojure.lang.IMapEntry
[map-entry idx]  
(do ;(println (val map-entry))
(val map-entry)))

(defmethod get-child com.sun.jdi.ThreadReference
[vmirror idx]
(.frame vmirror idx))

(defmethod get-child com.sun.jdi.StackFrame
[vmirror idx]
(let [values (try (.getValues vmirror (.visibleVariables vmirror))
                (catch Exception e nil))]
      ;_      (println values)] 
  (if values (with-meta (into {} values) {:tag :locals})
       "no local variable information" )))

(defmethod get-child com.sun.jdi.Value
[vmirror idx] (.type vmirror))

(defmethod get-child :simple
[vmirror idx] nil)

(defmethod get-child :default
[vmirror idx] nil)


;;;;;;;;;;;;;;;;JTree model and proxy;;;;;;;;;;;;;

(defn tree-model 
"creates a TreeModel that delegates to the get-child, get-child-count, and is-leaf multimethods"
[data]
  (proxy [javax.swing.tree.TreeModel] []
    (getRoot [] data)
    (getChild [parent index]
      (try
        (get-child parent index) (catch Exception e e)))
    (getChildCount [parent]
      (try
       (get-child-count parent) (catch Exception e 0)))
    (isLeaf [node]
      (try
        (is-leaf node) (catch Exception e true)))
    (valueForPathChanged [path newValue])
    (getIndexOfChild [parent child]
      -1)
    (addTreeModelListener [treeModelListener])
    (removeTreeModelListener [treeModelListener])))


(defn popup-mouse-adapter
"creates a MouseAdapter that only allows pop-up triggers to be processed. Calls popup-callback with the event"
[popup-callback]
(let [pop (fn pop [#^java.awt.event.MouseEvent mevent] (when (.isPopupTrigger mevent) (popup-callback mevent)))]
(proxy [MouseAdapter] [] 
      (mousePressed [mevent] (pop mevent))
      (mouseReleased [mevent] (pop mevent)))))

(defn get-tree-component
"return the component the user has clicked on in the tree"
[tree mevent]
(let   [x    (.getX mevent)
        y    (.getY mevent)
        path (.getPathForLocation tree x y)]
   (when path (.getLastPathComponent path))))

(defn tree-popup-callback
"for testing JTree MouseListener"
[tree]
(fn [mevent]  (println (get-tree-component tree mevent))))

(defn make-tree
"create a Jtree with a mouselistener that calls pop-back via popup-mouse-adapter"
([model pop-back] 
(let [tree (proxy [javax.swing.JTree] [model] 
            (convertValueToText [obj sel? exp? leaf? row hasFocus?]
                  (mirror-txt obj)))]
  (if pop-back (doto tree (.addMouseListener (popup-mouse-adapter pop-back)))
         tree)))
([model] (make-tree model nil))) 


(defn mirror-tree
"creates a JFrame containing a Jtree (created by make-tree) with the given data"
[data title]
(let [tree (make-tree (tree-model data))]
  (doto (new javax.swing.JFrame title)
    (.setDefaultCloseOperation javax.swing.JFrame/DISPOSE_ON_CLOSE)    
    (.add (new javax.swing.JScrollPane tree))
    (.setSize 400 600)
    (.setVisible true)))) 


;;;;;;;;;;;;;;;;Add breakpoints;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn filter-dialog
"creates a dialog for selecting a value from a combobox"
[app-state s msg] 
(let [txt-dim    (new Dimension 200,30)
      dlg     (doto (new javax.swing.JDialog (app-state :debug-frame) true) (.setTitle msg))
      cbox    (doto (new JComboBox (into-array Object s)) (.setMaximumSize (new Dimension 300,20)) (.setEditable false))
      done-fn  (fn[ok?] (let [results [ok? (.getSelectedItem cbox)]] 
                                     (dosync
                                                  (commute app-state assoc :filt-dlg-results results))
                                     (when dlg (.hide dlg))    ))]
  (doto dlg (.setContentPane (sw-box :y  cbox
                          20 (sw-box :x (sw-button "OK" (fn [e] (done-fn true))) 10 (sw-button "Cancel" (fn [e] (done-fn false))))))
             (.pack))))

(def meth-map    (ref (sorted-map)))
(def loc-map     (ref (sorted-map)))

(defn add-bpt-panel
[virtm disp-upd-fn]
(let [pref-dim    (new Dimension 200,300)
      
      
      erm         (.eventRequestManager virtm)
      loc-box      (doto (new JComboBox) (.setMaximumSize (new Dimension 300,20)))
      txtfield    (doto (new JTextField) (.setMaximumSize (new Dimension 300,20)))
      methlist    (sw-list '() nil)

      add-fn      (fn add-fn [e] (let [loc-descr (.getSelectedItem loc-box)          ;(.getSelectedValue methlist)
                                       loc       (@loc-map loc-descr)               ;(@meth-map meth-descr)
                                       bpt       (when loc (.createBreakpointRequest erm loc))]   ;(when meth (.createBreakpointRequest erm (.location meth)))]
                                    (.enable bpt)
                                    (disp-upd-fn)))
      add-button   (sw-button "add bpt" add-fn)
      list-sel    (fn list-sel [#^ListSelectionEvent e] 
                    (let [meth-descr (.getSelectedValue methlist)
                          meth       (@meth-map meth-descr)
                          ;_          (println meth-descr meth)
                          locs       (try (.allLineLocations meth) (catch Exception e (println e)))
                          loc-str    (doall (for [loc locs] (str (.sourceName loc) ":" (.lineNumber loc))))]
                      (dosync
                         (ref-set loc-map (apply sorted-map (interleave loc-str locs))))
                      (sw-cbox-replace-all loc-box loc-str)
                      (when (> (count locs) 0) (.setSelectedIndex loc-box 0)
                                               (.setEnabled add-button true))))

      _            (doto  methlist (.addListSelectionListener (proxy [ListSelectionListener] []
                                                                               (valueChanged [e] (list-sel e)))))
                                       

      upd-fn      (fn upd-fn [] (let [obj (into-array Object (keys @meth-map))]
                                 (.setListData methlist obj)))
      srch-fn     (fn srch-fn [e] (do
                                   (.setEnabled add-button false)
                                  (let [fn-name (.getText txtfield)
                                        meths   (get-meths2 virtm fn-name)] ;!!!!!!!!!get-meths2
                                    (dosync 
                                       (ref-set meth-map (meths->map meths)))
                                    (upd-fn))))

      srch-button  (sw-button "search" srch-fn)
      border       (BorderFactory/createTitledBorder (BorderFactory/createEtchedBorder)
                            "add breakpoint") ]
                                    
   (doto (sw-box :y 5 (new JLabel "function or method name") (sw-box :x txtfield 10 srch-button) 20
                           (new JLabel "matches")
                            (sw-box :x (new JScrollPane methlist) 10 (sw-box :y loc-box 10 add-button) ))  (.setPreferredSize pref-dim) 
                (.setBorder border))))

;breakpoints are stored as maps so that breakpoints with thread filtering can be displayed with the thread info. Other filtering 
;(available in the JPDA API but not currently in this implementation) could be handled similarly.
;

(defn sync-bpt-info
"called after adding or deleting a breakpoint, updates the :bpt-info map in app-state"
[app-state]
(let [vm       (@app-state :vm)
      erm      (.eventRequestManager vm)
      bpts     (.breakpointRequests erm)]
  (dosync
      (let [old-info (or (@app-state :bpt-info) {})
            bpt-info (reduce (fn[m b] (assoc m b (or (old-info b) (with-meta {:bpt b} {:type :bpt-info})))) {} bpts)]
       (commute app-state assoc :bpt-info bpt-info)
       bpt-info))))




(defn bpt-list-panel
"create a container for a list of current breakpoints and buttons for manipulating them"
[virtm app-state]
(let [pref-dim    (new Dimension 400,300)
      list-sel   (fn list-sel [lst e] (let [lst     (when e (.getSource e))
                                            sel-val (when lst (.getSelectedValue lst))]
                                          (when sel-val (dosync (commute app-state assoc :sel-bpt (sel-val :bpt))))))
      erm         (.eventRequestManager virtm)
      bptlist    (sw-list '() list-sel)
      upd-fn     (fn upd-fn [] (let [bpt-info (sync-bpt-info app-state)]
                                 (.setListData bptlist (into-array Object (vals bpt-info)))))

      rem-fn     (fn rem-fn [e] (let [bpt ((.getSelectedValue bptlist) :bpt)]
                                   (when bpt (.deleteEventRequest erm bpt))
                                   (upd-fn)))
      rem-button (sw-button "remove" rem-fn)
      enbl-fn     (fn enbl-fn [e] (let [bpt ((.getSelectedValue bptlist) :bpt)]
                                   (when bpt (.enable bpt))
                                   (upd-fn)))
      enbl-button (sw-button "enable" enbl-fn)
      dsbl-fn     (fn dsbl-fn [e] (let [bpt ((.getSelectedValue bptlist) :bpt)]
                                   (when bpt (.disable bpt))
                                   (upd-fn)))
      dsbl-button (sw-button "disable" dsbl-fn)
      thr-filt-fn  (fn thrd-filt-fn [e] (let [bpt #^com.sun.jdi.request.BreakpointRequest (@app-state :sel-bpt)]
                                           (when bpt
                                              (let [dlg       (filter-dialog app-state (.allThreads virtm) "select thread")
                                                      _       (doto dlg (.show) (.dispose))
                                                    [ok? thr] (@app-state :filt-dlg-results)
                                                    bpt-enbl? (.isEnabled bpt)]
                                                (when ok? (.disable bpt)
                                                          (.addThreadFilter bpt thr)
                                                          (dosync 
                                                              (commute app-state assoc-in [:bpt-info bpt :thr] thr))
                                                          (when bpt-enbl? (.enable bpt))
                                                          (upd-fn))))))
      thr-button (sw-button "thread filter..." thr-filt-fn)

      border       (BorderFactory/createTitledBorder (BorderFactory/createEtchedBorder)
                            "breakpoints") ]
   [(doto (sw-box :x  (new JScrollPane bptlist) 10 (sw-box :y rem-button 10 enbl-button 10 dsbl-button 10 thr-button))
                    (.setBorder border) (.setPreferredSize pref-dim))
     upd-fn]))

(comment


(def vmm (.  com.sun.jdi.Bootstrap (virtualMachineManager)))
(def defcon (. vmm (defaultConnector)))
(def vm (. defcon (launch (con-args defcon "clojure.lang.Repl" "-cp clojure.jar;./classes;./lib/*"))))
(def vmq-vec (vmq->sq vm))
(def vm-proc (. vm (process)))
(def proc-thrs (proc-gui "test" vm-proc))
;(def erm (.eventRequestManager vm))
(. vm (resume))
(get-event (second vmq-vec)) ;start-up event



)

;sync execution in debugee - no longer used

(defn mirror-thr-exec
"attempts to execute the function exec-fn requiring a thread in the debugee for method invocation. Returns [success? result]
where success? is true if exec-fn was executed without exception and result is the return value from exec-fn. When success?
is nil, result is a string message or exception. exec-fn takes two arguments, the virtual machine mirror and a debugee thread reference."
[app-state exec-fn]
(try
   (let [virtm   (@app-state :vm)
         bpt     (@app-state :bpt-event)
         thr     (when bpt (.thread #^com.sun.jdi.event.LocatableEvent bpt))
         pre-fn  (when thr (@app-state :pre-eval))
         post-fn (when thr (@app-state :post-eval))
         res     (when thr (do (when pre-fn (pre-fn))
                         (exec-fn virtm thr)))]
       (when post-fn (post-fn))
       (if thr [true res]
               [nil "no thread available - set a breakpoint"]))
   (catch Exception e [nil e])))


;async execution is chosen to avoid hanging the debugger when the debugee hangs or is given a long-running task

(def debugee-exec-agent (agent nil))
;(clear-agent-errors debugee-exec-agent)


(defn mirror-thr-async
"attempts to execute the function exec-fn requiring a thread in the debugee for method invocation. Local execution of exec-fn is asynchronous. 
Returns [success? result] where success? is true if exec-fn was scheduled for execution and result is a string message. When success?
is nil, result is a string message or exception. exec-fn takes two arguments, the virtual machine mirror and a debugee thread reference."
[app-state exec-fn]
(try
   (let [virtm   (@app-state :vm)
         bpt     (@app-state :bpt-event)
         thr     (when bpt (.thread #^com.sun.jdi.event.LocatableEvent bpt))
         pre-fn  (when thr (@app-state :pre-eval))
         post-fn (when thr (@app-state :post-eval))
         ag-fn   (fn ag-fn [_] (let [res (when thr (exec-fn virtm thr))]
                                   (sw-later post-fn)
                                   res))                                               
         _       (when thr (when pre-fn (pre-fn))
                         (send-off debugee-exec-agent ag-fn)) ]
       (if thr [true "executing..."]
               [nil "no thread available - set a breakpoint"]))
   (catch Exception e [nil e])))

;;;!!!!ASYNC VERSION

;TO-DO in case of exception, display tree for debugee exception: (.exception result)
(defn eval-panel
[virtm app-state]
(let [[doc lstnr]  (parmatch-doc)
      pref-dim    (new Dimension 400,300)
      entry        (doto (no-wrap-text-pane doc) (.addCaretListener lstnr))
      scr-entry    (new JScrollPane entry) 
      eval-fn      (fn eval-fn [e] (let [button  (.getSource e)
                                         _       (doto button (.setEnabled false))
                                         txt    (.getText entry)
                                         title   (str "eval results from " 
                                                   (if (> (.length txt) 20) (str (.substring txt (int 0) (int 20)) "...")
                                                              txt))
                                         async-fn (fn async-fn [vm thr] (let [ev-fn (fn [] (try (mirror-eval vm thr txt) (catch Exception e e)))
                                                                              res    (around-invoke virtm ev-fn)]
                                                                            (sw-later #(do (.setEnabled button true)
                                                                                           (mirror-tree res title)))
                                                                              res))
                                         [success? msg] (mirror-thr-async app-state async-fn)
                                         frame           (@app-state :debug-frame)]
                                     (when (not success?) (JOptionPane/showMessageDialog frame (str msg))
                                                          (dosync (commute app-state assoc :eval-exc msg))
                                                           (.setEnabled button true))))                                   
      eval-button  (sw-button "eval" eval-fn)
      border       (BorderFactory/createTitledBorder (BorderFactory/createEtchedBorder)
                            "eval in debugee") ]
  (.. scr-entry (getViewport) (setBackground  java.awt.Color/WHITE))
;the following is needed due to no-wrap to allow user to click anywhere in the pane and get focus
  (.. scr-entry (getViewport) (addMouseListener (proxy [MouseAdapter] [] 
                                    (mousePressed [mevent]
                                        (. entry (requestFocusInWindow))))))
  (doto (sw-box :y  scr-entry 5 eval-button) (.setBorder border) (.setPreferredSize pref-dim))))
;!!!!!!!!!!!!!!!



(defn set-bpt-state
"updates the app-state with a BreakpointEvent (keyed to :bpt-event) if it is contained in the EventSet e.
Returns the breakpoint event."
[#^com.sun.jdi.event.EventSet e app-state]
(let [obj (into-array Object (.toArray e))
      bpt (last (filter #(or(instance? com.sun.jdi.event.StepEvent %) (instance? com.sun.jdi.event.BreakpointEvent %)) obj))]
  (when bpt (dosync
              (commute app-state assoc :bpt-event bpt))
        bpt)))


(defn txt-frame
[data title]
  (doto (new JFrame title)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)    
    (.add (new JScrollPane (new JTextArea (str data))))
    (.setSize 400 600)
    (.setVisible true)))

(defn def-dialog
[app-state] 
(let [txt-dim    (new Dimension 200,30)
      dlg     (doto (new javax.swing.JDialog (app-state :debug-frame) true) (.setTitle "def in debugee"))
      ns-txt  (doto (new javax.swing.JTextField) (.setPreferredSize txt-dim))
      var-txt  (doto (new javax.swing.JTextField) (.setPreferredSize txt-dim))
      done-fn  (fn[ok?] (let [results [ok? (.getText ns-txt) (.getText var-txt)]]
                                     (.setText ns-txt "") (.setText var-txt "")
                                     (dosync
                                         (commute app-state assoc :def-dlg-results results))
                                     (when dlg (.hide dlg))))]
  (doto dlg (.setContentPane (sw-box :y (sw-box :x 
                                (sw-box :y  (new JLabel "namespace") 10 ; (int JLabel/RIGHT_ALIGNMENT) ?
                                             (new JLabel "var name")) 10 
                                 (sw-box :y ns-txt 10 var-txt))
                          20 (sw-box :x (sw-button "OK" (fn [e] (done-fn true))) 10 (sw-button "Cancel" (fn [e] (done-fn false))))))
             (.pack)))) 

(defn validate-def-txt
[ns-txt var-txt]
(let [ns-ok? (and ns-txt (> (.length ns-txt) 0))
      var-ok? (and var-txt (> (.length var-txt) 0))]
  [ns-ok? var-ok?]))  
           
;(defn def-mirror
;[virtm app-state mirror] (println mirror))



;;;;multimethod for extracting info from a debugee event


(defmulti get-event-info class)

(defmethod get-event-info com.sun.jdi.event.EventSet
[obj] 
(map get-event-info (seq obj)))
      

(defmethod get-event-info com.sun.jdi.event.MethodExitEvent
[#^com.sun.jdi.event.MethodExitEvent  obj] 
{:method (.method obj) :return (.returnValue obj)})

(defmethod get-event-info :default
[obj] 
(str obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-mer-thread
[thr-name]
(let [mer #^com.sun.jdi.request.MethodExitRequest (@appstate :mer)
      thr (thread-by-name (@appstate :vm) thr-name)]
  (.setEnabled mer false)
  (.addThreadFilter mer thr)
  (.setEnabled mer true)))

;
;
(defn events-panel
"creates the gui container for debugger event handling: suspend/resume/step, and tree display of stack frames"
[virtm app-state]
(let [erm         (.eventRequestManager virtm)
      last-step   (ref nil)
      delete-step (fn del-step [] (dosync
                                         (when @last-step (.deleteEventRequest erm @last-step)
                                                    (ref-set last-step nil))))
      pref-dim      (new Dimension 300,300)
      list-sel      (fn list-sel [lst e] ) ;no-op for now
      event-count   (ref 0)
      events         (sw-list '() list-sel)
                     ;(doto (sw-list '() list-sel) (.setPreferredSize pref-dim))

;def in debugee
      def-dlg     (def-dialog app-state)

;!!!ASYNC def'ing
      def-mirror  (fn def-mir [obj] (do 
                                     (.show def-dlg) 
                                     (let [[ok? ns-txt var-txt] (@app-state :def-dlg-results)]
                                         (when ok? 
                                             (let [[ns-ok? var-ok?] (validate-def-txt ns-txt var-txt)
                                                    ns-msg          (when (not ns-ok?) "namespace not specified")
                                                    var-msg         (when (not var-ok?) "var name not specified")
                                                    frame           (@app-state :debug-frame)
                                                    async-fn        #(let [def-fn (fn [] (try (mirror-def %1 %2 obj #^String ns-txt #^String var-txt)
                                                                                    (str "def'ed " ns-txt "/" var-txt)
                                                                                    (catch Exception e e)))
                                                                           res     (around-invoke virtm def-fn)]
                                                                       (sw-later (fn [] (JOptionPane/showMessageDialog frame res)))
                                                                        res)]
                                               (if (or ns-msg var-msg) 
                                                     (JOptionPane/showMessageDialog frame (str ns-msg \newline var-msg))
                                                   (let [[success? def-res] 
                                                                    (mirror-thr-async app-state async-fn)]
                                                     (when (not success?) 
                                                                (dosync (commute app-state assoc :mir-def-exc def-res))
                                                              (JOptionPane/showMessageDialog frame (str "def failed:" \newline def-res))))))))))
;!!!!!!!!!



;pop-up menu for thread tree
      pop-obj      (ref nil) 
      pop-def-item  (new JMenuItem "def in debugee...")
      pop-listen   (proxy [ActionListener] []
                       (actionPerformed [e] (dosync
                                              (let [src (.getSource e)]
                                                (when (and (= src pop-def-item) (instance? com.sun.jdi.Mirror @pop-obj))
                                                   (def-mirror @pop-obj)
                                                     (ref-set pop-obj nil))))))                                         
      pop-menu     (doto (new JPopupMenu) (.add (doto pop-def-item (.addActionListener pop-listen)))) 

 
 ;thread-tree
      thr-tree     (make-tree (tree-model "Thread info not available"))
      pop-back     (fn[e] (dosync
                                (let [obj  (get-tree-component thr-tree e)]
                                 (when (instance? com.sun.jdi.Mirror obj) 
                                           (ref-set pop-obj obj)
                                           (.show pop-menu (.getComponent e) (.getX e) (.getY e))))))   
      thr-tree     (doto thr-tree (.addMouseListener (popup-mouse-adapter pop-back)))

;event list actions
;       pop-back2   (fn[e] (dosync
;                              (let [obj (.getSelectedValue events)
                                    

;update components
      upd-tree     (fn upd-tree [] (.setModel thr-tree (tree-model 
                                                         (with-meta (seq (.allThreads virtm)) 
                                                               {:tag :threads}))))
      blank-tree   (fn blk-tree [] (.setModel thr-tree (tree-model "Thread info not available")))
      count-event  (fn [] (dosync (ref-set event-count (+ @event-count 1)))) 
      add-event-set (fn add-e-s [#^com.sun.jdi.event.EventSet es]
                         (let [model #^javax.swing.DefaultListModel (.getModel events)]
                           (doseq [e es]
                            (when e
                             (let [msg (print-str (count-event) (.toString e))]
                               (.addElement model msg)
                               (.setSelectedValue events msg true) )))))
      add-event-msg (fn add-e-m [obj]
                         (let [model #^javax.swing.DefaultListModel (.getModel events)
                               msg (str obj)]
                            (.addElement model msg)
                            (.setSelectedValue events msg true)))                                
      callback  (fn callback [e] (do
                                   (if (instance? com.sun.jdi.event.EventSet e)
                                     (let [bpt (set-bpt-state e app-state)]
                                       (dosync (commute app-state assoc :cur-event e))
                                       (sw-later #(add-event-set e))
                                       (when bpt (upd-tree)))
                                     (sw-later #(add-event-msg e)))
                                   nil))
                                 
      kill-fn      (proc-events virtm callback)
      exit-fn      (fn exit-fn [] (try (kill-fn) (.exit virtm 0) (catch Exception e e)))
      susp-button  (sw-button "suspend" (fn sus-fn [e] (do (.suspend virtm) (upd-tree))))
      res-button   (sw-button "resume" (fn res-fn [e] (do  (blank-tree) 
                                                           (dosync
                                                             (commute app-state assoc :bpt-event nil))
                                                           (.resume virtm))))
      step-fn      (fn step-fn [size depth _]   ;last arg is a swing event object we don't need
                        (let [bpt  (@app-state :bpt-event)
                              thr  (when bpt (.thread #^com.sun.jdi.event.LocatableEvent bpt))
                              stp  (when thr (try
                                               (delete-step)
                                               (step virtm thr size depth)
                                              (catch Exception e (callback e))))]
                               (when (not thr) (add-event-msg (str "[no thread for stepping]" \newline)))
                               (when stp (dosync (ref-set last-step stp)))))
      stpin-button (sw-button "step in" #((partial step-fn :line :into) %))
      stpout-button (sw-button "step out" #((partial step-fn :line :out) %))
      stpov-button (sw-button "step over" #((partial step-fn :line :over) %))
]
  (dosync (commute app-state assoc :pre-eval blank-tree :post-eval upd-tree :exit-fn exit-fn :step-fn step-fn))
  (sw-split :h (sw-box :y (new JScrollPane events) 10 (sw-box :x susp-button res-button stpin-button stpout-button stpov-button))
                            
             (doto  (new JScrollPane thr-tree) (.setPreferredSize pref-dim)))))

(defn debug-gui
"create the gui frame for the debugger"
[virtm title app-state]
(let [events-pnl         (events-panel virtm app-state)
      [bpt-panel bpt-fn] (bpt-list-panel virtm app-state)
      bpt-add-panel      (add-bpt-panel virtm bpt-fn)
      eval-pnl           (eval-panel virtm app-state)
      main-fr            (sw-frame title (new Dimension 900 700) 
                              (sw-split :v events-pnl (sw-split :h (sw-box :y bpt-add-panel 30 bpt-panel) eval-pnl)))]
  (dosync (commute app-state assoc :debug-frame main-fr :bpt-upd bpt-fn))
  main-fr))


(defn frame-closer
"create a WindowListener that calls the supplied functions when user triggers a closing"
[closing-fn closed-fn]
 (proxy [WindowListener] []
                   (windowActivated [e])
			 (windowClosed [e] (when closed-fn (closed-fn)))
                   (windowClosing [e](when closing-fn (closing-fn)) )
                   (windowDeactivated [e])
                   (windowDeiconified [e])
                   (windowIconified [e])
                   (windowOpened [e])))

(defn exit-fn
"shutdown the debugger - exit the debugee vm and close windows."
[app-state]
(let [run?     (ref true) ;Runs closing actions only once, otherwise swing/awt thread will inf loop due to dispose calls
      ex-fn    (fn ex-fn [](dosync
                             (when @run?
                               (let [proc-frame    (@app-state :proc-frame)
                                  debug-frame   (@app-state :debug-frame)
                                  ex-fn         (@app-state :exit-fn)]
                                (try (ex-fn) (catch Exception e))
                                (when proc-frame (.dispose proc-frame))
                                (when debug-frame (.dispose debug-frame)))
                                 (ref-set run? false))))]
         ex-fn))

;current implementation only allows one debugger instance to run at a time. This could be changed by introducing multiple appstates.

(defn debug
"start a debugger with two frames, one for process interaction, the other for controlling the debugger"
[main options]
(do
(dosync
(let [prev-exit  (@appstate :exit-fn)]
  (when prev-exit (prev-exit)); shut down the current debugger instance if it's still around
  (ref-set appstate {})))
(let [vmm        (com.sun.jdi.Bootstrap/virtualMachineManager)
      defcon     (.defaultConnector vmm)
      vm         (.launch defcon  (con-args defcon main options))
      vm-proc    (.process vm)
      exit-f     (exit-fn appstate)
      fr-closer  (frame-closer nil exit-f)]
   (dosync (commute appstate assoc :vm vm))
   (sw-later  #(do 
                  (.addWindowListener (proc-gui (str "[debug process]" main) vm-proc appstate) fr-closer)
                       
                  (.addWindowListener (debug-gui vm (str "[debug]" main) appstate) fr-closer))))))

;(.defaultArguments (first (into [] (.attachingConnectors (com.sun.jdi.Bootstrap/virtualMachineManager)))))
;(.name (first (into [] (.attachingConnectors (com.sun.jdi.Bootstrap/virtualMachineManager)))))
;(.description (first (into [] (.attachingConnectors (com.sun.jdi.Bootstrap/virtualMachineManager)))))

      
              

(defn debug-ext
"start a debugger for an external vm with options -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=port"
[port]
(do
(dosync
(let [prev-exit  (@appstate :exit-fn)]
  (when prev-exit (prev-exit)); shut down the current debugger instance if it's still around
  (ref-set appstate {})))
(let [a-c (first (filter #(> (.indexOf (.name %)"Socket" 0) -1 ) 
                 (.attachingConnectors (com.sun.jdi.Bootstrap/virtualMachineManager))))
      def-args (.defaultArguments a-c)
      _        (.setValue (.get def-args "hostname") "localhost")
      _        (.setValue (.get def-args "port") "8030")
      ;(println def-args)
      vm (.attach a-c def-args)
      exit-f     (exit-fn appstate)
      fr-closer  (frame-closer nil exit-f)]
   (dosync (commute appstate assoc :vm vm))
   (sw-later  #(do     
                  (.addWindowListener (debug-gui vm (str "[debug]" port) appstate) fr-closer))))))

;TO-DO's
;proper shutdown with close dialog 
;
;connect to previously running jvm; start jvm using cmdline like this:
;-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8030
(comment

(load "/jge/jdi/gui")
(in-ns 'jge.jdi.gui)
(refer  'jge.swing utils)
(refer  'jge.jdi base)


(def vmm (.  com.sun.jdi.Bootstrap (virtualMachineManager)))
(def defcon (. vmm (defaultConnector)))
(def vm (. defcon (launch (con-args defcon "clojure.main" "-cp ./src:./classes:./lib/*"))))
;(def vmq-vec (vmq->sq vm))
(def vm-proc (. vm (process)))
(def proc-thrs (proc-gui "test" vm-proc appstate))
(def erm (.eventRequestManager vm))
(. vm (resume))
;(get-event (second vmq-vec)) ;start-up event



)
