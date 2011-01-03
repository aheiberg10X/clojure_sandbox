(ns highlife.graphics
  (:gen-class)
  (:require [highlife.parameters :as params]
            [highlife.goant :as goant]
            [highlife.moveactions :as mas])
  (:import (java.awt Color Dimension Graphics GridBagLayout GridBagConstraints)
           (javax.swing JButton JPanel JFrame Timer JOptionPane JMenu JMenuBar JMenuItem)
           (java.awt.event ActionListener MouseListener)
           (java.lang Thread))
  (:use clojure.contrib.except))

(def point-size 8)
(def generation-length-millis 20)

(def initial-seed (int (* params/DIM params/DIM 0.20))) ; initial population is 20%

;;these functions translate between grid coordinates and display coordinates
(defn scale-point-for-display [pt]
  "Returns a vector of [x y width height] for the point to be drawn"
  (concat (map #(* % (+ 1 point-size)) pt) [point-size point-size]))

(defn display-to-grid-point [pt]
  "Takes pixel coordinates and Returns a vector of [x y] coresponding to a grid cell"
  (map #(int (/ 1 (/ (+ 1 point-size) %))) pt))

;memoize coordinate functions
(def m-scale-point-for-display (memoize scale-point-for-display))
(def m-display-to-grid-point (memoize display-to-grid-point))

;running state
(def paused? (atom false))
(def running? (atom true))

;;drawing functions
(defn fill-point [#^Graphics g pt  #^Color color]
  (let [[x y width height] (m-scale-point-for-display pt)]
    (doto g
      (.setColor color)
      (.fillRect x y width height))))

(defn paint [#^Graphics g color pt]
  (fill-point g pt color))

(defn get-color [ws-ref pt]
  (let [[whowhere whatwhere whos] (goant/expand-worldstate @ws-ref)]
    (if (get whowhere pt)
      (Color. 0 0 0)
      (if (> (mas/pheremone-level (get-in whatwhere pt)) 0)
        (Color. 244 248 48)
        (Color. 255 255 255)))))

;simualtion panel
(defn sim-panel [ws-ref]
  (proxy [JPanel MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent #^Graphics g)                    
      (let [[whowhere whatwhere whos] (goant/expand-worldstate @ws-ref)]
        (dorun
         (map #(paint g (% 0) (% 1))
              (dosync (doall
                       (for [x (range params/DIM) y (range params/DIM)]
                         [(get-color ws-ref [x y]) [x y]])))))))
    (mouseClicked [e]
      ;; (let [pt (m-display-to-grid-point [(.getX e) (.getY e)]) cell (get-in grid pt) alive? (:alive? @cell)]
      ;;   (dosync (alter cell assoc :alive? (not alive?)))
      ;;   (fill-point (.getGraphics this) pt (if (not alive?) (Color. 255 0 0) (Color. 255 255 255))))
      )
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])
    (getPreferredSize []
      (let [dim params/DIM]
        (Dimension. (+ dim (* dim point-size)) (+ dim (* dim point-size)))))))

;;buttons
(defn seed-grid-button [grid panel]
  (proxy [JButton ActionListener] ["Seed Grid"]
    (actionPerformed [e]
      ;;(seed-grid initial-seed grid)
      (.repaint panel))))

(defn clear-grid-button [grid panel]
  (proxy [JButton ActionListener] ["Clear Grid"]
    (actionPerformed [e]
      ;;(clear-grid grid)
      (.repaint panel))))

(defn pause-life-button [sim-thread]
  (proxy [JButton ActionListener] ["Pause Simulation"]
    (actionPerformed [e]
      (swap! paused? not)
      (if (not @paused?) 
        (.setText this "Pause Simulation")
        (.setText this "Resume Simulation")))))

;;using the 'x' button in the window is not killing the running thread
;;can do this using windowListener or something, but screw it
;;made this button to stop the thread
(defn make-interrupt-button []
  (proxy [JButton ActionListener] ["interrupt thread"]
    (actionPerformed [e]
      (swap! running? not)
      (if @running?
        (.setText this "Interrupt please")
        (.setText this "ohhhh snap")))))

;;update thread
;;sim-panel will be my worldstate-ref
(defn create-sim-thread [worldstate-ref panel]
  (Thread.
   #(let [dim params/DIM
          grid-seq (for [x (range dim) y (range dim)] (get-in worldstate-ref [x y]))]
      (loop [gen 1]
        (if (not @running?)
          (.interrupt (Thread/currentThread))
          (if @paused?
            (do
              ;;(.interrupt (Thread/currentThread))
              (Thread/sleep 300)
              (recur gen)
              )
            (do
              (dosync (alter worldstate-ref goant/make-moves))
              (.repaint panel)
              (Thread/sleep generation-length-millis)
              (recur (inc gen))))))))
)

(defn test []
  (let [frame (JFrame. "Goant")
        ws-ref (goant/make-worldstate-ref params/DIM 4)
        panel (sim-panel ws-ref)
        sim-thread (create-sim-thread ws-ref panel)
        pause-button (pause-life-button sim-thread)
        interrupt-button (make-interrupt-button)
        
        layout (GridBagLayout.)
        pause-button-constraints (GridBagConstraints.)
        interrupt-button-constraints (GridBagConstraints.)
        panel-constraints (GridBagConstraints.)]
    
    (.setDefaultCloseOperation frame 2)
    (.setDaemon sim-thread true)
    
    (.addActionListener pause-button pause-button)
    (set! (. pause-button-constraints gridx) 0)
    (set! (. pause-button-constraints gridy) 0)
    (.addActionListener interrupt-button interrupt-button)
    (set! (. interrupt-button-constraints gridx) 0)
    (set! (. interrupt-button-constraints gridy) 1)
    
    (doto panel
      (.setFocusable true)
      (.addMouseListener panel))
    (doto frame
      (.setLayout layout)
      (.add pause-button pause-button-constraints)
      (.add interrupt-button interrupt-button-constraints)
      (.add panel panel-constraints)
      (.pack)
      (.setVisible true))
    (.start sim-thread)))

;simulation start function - call this to make things go
;; (defn start []
;;   (let [sim-grid (build-grid grid-dim)
;;         frame (JFrame. "A Clojure Highlife")
;;         panel (sim-panel frame sim-grid)
;;         sim-thread (create-sim-thread sim-grid panel)
;;         seed-button (seed-grid-button sim-grid panel)
;;         clear-button (clear-grid-button sim-grid panel)
;;         pause-button (pause-life-button sim-thread)

;;         layout (GridBagLayout.)
;;         seed-button-constraints (GridBagConstraints.)
;;         clear-button-constraints (GridBagConstraints.)
;;         pause-button-constraints (GridBagConstraints.)

;;         panel-constraints (GridBagConstraints.)]

;;     ;populate initial grid
;;     (seed-grid initial-seed sim-grid)
    
;;     ;action listener setup
;;     (.addActionListener seed-button seed-button)
;;     (.addActionListener clear-button clear-button)
;;     (.addActionListener pause-button pause-button)

;;     ;layout contraint definitions
;;     (set! (. seed-button-constraints gridx) 0)
;;     (set! (. seed-button-constraints gridy) 0)
;;     (set! (. clear-button-constraints gridx) 1)
;;     (set! (. clear-button-constraints gridy) 0)
;;     (set! (. pause-button-constraints gridx) 2)
;;     (set! (. pause-button-constraints gridy) 0)
;;     (set! (. panel-constraints gridx) 0)
;;     (set! (. panel-constraints gridy) 1)
;;     (set! (. panel-constraints gridwidth) 3)

;;     (doto panel
;;       (.setFocusable true)
;;       (.addMouseListener panel))

;;     (doto frame
;;       (.setLayout layout)
;;       (.add seed-button seed-button-constraints)
;;       (.add clear-button clear-button-constraints)
;;       (.add pause-button pause-button-constraints)
;;       (.add panel panel-constraints)
;;       (.pack)
;;       (.setVisible true))
;;     (.start sim-thread))) ;sim starts here


;; (defn -main
;;   ([] ;if no args are provided, use defaults
;;     (start))
;;   ([dims seed] ;modify defaults by passing args
;;     (def grid-dim (Integer/parseInt dims)) ;user-defined grid dimensions
;;     (def initial-seed (Integer/parseInt seed)) ;user-defined initial seed
;;     (start)))
