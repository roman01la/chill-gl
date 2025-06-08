(ns cgl.camera
  (:import [org.joml Matrix4f Vector3f]))

(defrecord Camera [position target up])

(defn create-camera
  "Creates a new camera with the given position, target, and up vector"
  ([]
   (create-camera (Vector3f. 0.0 0.0 3.0)
                  (Vector3f. 0.0 0.0 0.0)
                  (Vector3f. 0.0 1.0 0.0)))
  ([position target up]
   (->Camera position target up)))

(defn get-view-matrix
  "Returns the view matrix for this camera"
  [camera]
  (let [{:keys [position target up]} camera]
    (-> (Matrix4f.)
        (.identity)
        (.lookAt position target up))))

(defn move-camera
  "Move the camera by the given delta values"
  [camera dx dy dz]
  (let [position (:position camera)]
    (.add position dx dy dz)
    camera))

(defn move-forward
  "Move the camera forward by the given amount"
  [camera amount]
  (let [direction (-> (Vector3f.)
                      (.set ^Vector3f (:target camera))
                      (.sub (:position camera))
                      (.normalize)
                      (.mul amount))]
    (move-camera camera (.x direction) (.y direction) (.z direction))))

(defn move-right
  "Move the camera right by the given amount"
  [camera amount]
  (let [direction (-> (Vector3f.)
                      (.set ^Vector3f (:target camera))
                      (.sub (:position camera))
                      (.cross (:up camera))
                      (.normalize)
                      (.mul amount))]
    (move-camera camera (.x direction) (.y direction) (.z direction))))

(defn rotate-camera
  "Rotate the camera around the y axis by the given angle in radians"
  [camera angle]
  (let [position (:position camera)
        target (:target camera)
        direction (-> (Vector3f.)
                      (.set ^Vector3f position)
                      (.sub target))
        distance (.length direction)

        ; Calculate new x and z positions
        new-x (* distance (Math/sin angle))
        new-z (* distance (Math/cos angle))

        ; Create new position
        new-position (Vector3f. new-x (.y position) new-z)]

    (assoc camera :position new-position)))
