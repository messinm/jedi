
;
;   Copyright (c) April 2009 Mike Messinides. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;
;   You must not remove this notice, or any other, from this software.
;
(ns messinm.jedi.swing.utils

(:import (javax.swing JFrame JLabel JTextField JTextPane JButton JTextArea JPanel JScrollPane JSplitPane JList SwingConstants JComboBox
                      Box BoxLayout ListSelectionModel UIManager)  
        (javax.swing.event ListSelectionListener)
	  (javax.swing.tree TreeModel) 
        (javax.swing.event CaretEvent CaretListener)        
        (javax.swing.text DefaultStyledDocument StyleConstants StyleConstants$CharacterConstants SimpleAttributeSet)
        (java.awt.event ActionListener WindowListener MouseAdapter) 
        (java.awt GridLayout FlowLayout Component Dimension Font EventQueue)))

(defn sw-later
[r] (javax.swing.SwingUtilities/invokeLater r))

(defn sw-frame
[title & comp]
(let [frame (new JFrame title)]
  (.setSize frame 700 700)
  (doseq [c comp]
     (when (instance? javax.swing.JComponent c) (.add frame c))
     (when (instance? Dimension c) (.setSize frame c)))
  (doto frame (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE) 
                (.setVisible true))))

(defn sw-panel
[& comp]
(let [pnl (new JPanel)]
  (doseq [c comp]
     (.add pnl c))
 pnl))

(def split-const {:v JSplitPane/VERTICAL_SPLIT, :h JSplitPane/HORIZONTAL_SPLIT})

(defn sw-split
[type c0 c1] (new JSplitPane (split-const type) c0 c1))

(defn sw-immutable-list
[s f] 
(let [tlist  (new JList (into-array Object s))
      p      (proxy [ListSelectionListener] []
                 (valueChanged [e] (f e)))]
  (doto tlist (.addListSelectionListener p)
              (.setSelectionMode ListSelectionModel/SINGLE_SELECTION))))

(defn sw-list
([s f] 
(let [model  (new javax.swing.DefaultListModel)
      tlist  (new JList model)
      p      (when f (proxy [ListSelectionListener] []
                 (valueChanged [e] (f tlist e))))]
  (doseq [o s]
     (.addElement model o))
  (when p (.addListSelectionListener tlist p))
  (doto tlist 
              (.setSelectionMode ListSelectionModel/SINGLE_SELECTION))))
([s] (sw-list s nil)))



(def box-const {:x BoxLayout/X_AXIS, :y BoxLayout/Y_AXIS, :line BoxLayout/LINE_AXIS,
                :page BoxLayout/PAGE_AXIS})
(defn sw-box
"creates a swing Box component with components comp added. axis is one of :x, :y .
components can either be JComponents, or an integer. An integer will create rigid space with width or height
equal to the integer."
[axis & comp]
(let [box (new Box (box-const axis))]
  (doseq [c comp]
     (if (instance? javax.swing.JComponent c) (.add box c)
        (.add box (Box/createRigidArea (if (= axis :x) (new Dimension c 1) (new Dimension 1 c))))))
  box))

(defn sw-button
[label f]
(doto (new JButton label)
        (.addActionListener 
           (proxy [ActionListener] [] 
                (actionPerformed [evt] (f evt))))))

(defn sw-lbl-txtarea
[txtarea title]
(let [label (new JLabel title)
      pane  (new JPanel)]
    (doto pane (.setLayout (new BoxLayout pane BoxLayout/PAGE_AXIS)) (.add label) 
                                                   (.add txtarea))))

(defn sw-cbox-replace-all
[#^JComboBox cbox s]
(do (.removeAllItems cbox)
    (doseq [item s]
      (.addItem cbox item))))

(comment
(sw-frame "box" (sw-box :y (new JLabel "x0") (new JLabel "x1") (new JLabel "x2")))

(sw-frame "test" (sw-box :y 5 (sw-box :x (doto (new JTextField) (.setMaximumSize (new Dimension 300,20)))
                                       10 (sw-button "search" #(println "search" %))) 20
                            (sw-box :x (new JScrollPane (sw-list (range 100) #(println "list" %))) 10 
                                       (sw-button "add bpt" #(println "add" %) ))))
)

;;;;;;;;;;;;;;;;;;styled doc for paren, brace and bracket highlighting;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def plain-attr (new SimpleAttributeSet))
(def bold-attr (new SimpleAttributeSet))
(. StyleConstants (setBold bold-attr true))
(. StyleConstants (setBackground bold-attr (. java.awt.Color CYAN)))
;(. StyleConstants (setUnderline bold-attr true))
(defn closing? [#^String c] (or (= c ")") (= c "]") (= c "}")))
(defn match? [#^String cls #^String opn] (cond 
                                            (= cls ")") (= opn "(")
                                            (= cls "]") (= opn "[")
                                            (= cls "}") (= opn "{") 
                                             :default    false))

(defn parmatch-doc
"returns a StyledDocument and CaretListener that implement paren, bracket and brace highlighting"   
[]
(let [doc             (new  DefaultStyledDocument)
      new-char-offset (ref -1)
      listnr    (proxy [CaretListener, Runnable] []
                    (caretUpdate [#^CaretEvent e] 
                          (let [charOffset (- (. e (getDot)) 1)
                                c-char     (if (>= charOffset 0) (. doc (getText charOffset 1)))     
                                newOffset  (if (and c-char (> charOffset 0) (closing? c-char)) 
                                               (loop [depth 0 openOffset (- charOffset 1)]						     
                                                 (if (< depth 0) (+ openOffset 1)
                                                               (if (= openOffset -1) -1 
                                                                  (let [xchar (. doc (getText openOffset 1))]
                                                                    (recur (if (= xchar c-char) (+ depth 1) 
                                                                           (if (match? c-char xchar) (- depth 1) depth))
                                                                          (- openOffset 1) )))))
                                                                           -1)]
                                                     (dosync (ref-set new-char-offset newOffset)))
                                                      ;(println "charOffset,c-char, newOffset:" charOffset c-char newOffset))
                                                     (. EventQueue (invokeLater this)))
                                          (run [] (dosync 
                                                     ;(println "new:" @new-char-offset) 	
                                                    (. doc (setCharacterAttributes 0 (. doc (getLength)) plain-attr true))
                                                    (if (>= @new-char-offset 0) (do (. doc (setCharacterAttributes @new-char-offset 1 bold-attr true))))
                                                    (ref-set new-char-offset -1))))]
                     (list doc listnr)))

(defn no-wrap-text-pane
[doc] (proxy [JTextPane] [doc]
        (getScrollableTracksViewportWidth [] false)))

