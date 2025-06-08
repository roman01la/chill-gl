(ns cgl.mesh
  (:require [cgl.shader :as shader])
  (:import (java.nio FloatBuffer IntBuffer)
           (org.joml Matrix4f)
           (org.lwjgl BufferUtils)
           (org.lwjgl.opengl GL33)))

(defn- rotation* [model {:keys [x y z]}]
  (cond-> model
          x (.rotationX x)
          y (.rotationY y)
          z (.rotationZ z)))

(defn- rotate* [model {:keys [x y z]}]
  (cond-> model
          x (.rotateX x)
          y (.rotateY y)
          z (.rotateZ z)))

(defn ^FloatBuffer create-vertices-buffer [vertices]
  (-> (BufferUtils/createFloatBuffer (count vertices))
      (.put vertices)
      (.flip)))

(defn ^IntBuffer create-indices-buffer [indices]
  (-> (BufferUtils/createIntBuffer (count indices))
      (.put indices)
      (.flip)))

(defprotocol IRenderable
  (render [this]))

(defprotocol ITransformable
  (rotation [this {:keys [x y z] :as rotation}])
  (rotate [this {:keys [x y z] :as rotation-dt}]))

(defrecord Mesh [vao vbo ebo indices model shader]
  IRenderable
  (render [this]
    (shader/set-uniform :m4f shader :model model)
    (GL33/glBindVertexArray vao)
    (GL33/glDrawElements GL33/GL_TRIANGLES (count indices) GL33/GL_UNSIGNED_INT 0))

  ITransformable
  (rotation [this {:keys [x y z] :as rotation}]
    (rotation* model rotation))
  (rotate [this {:keys [x y z] :as rotation-dt}]
    (rotate* model rotation-dt)))

(defn create-mesh [{:keys [vertices indices shader]}]
  (let [vbo (GL33/glGenBuffers)
        vao (GL33/glGenVertexArrays)
        ebo (GL33/glGenBuffers)

        vertices-buffer (create-vertices-buffer vertices)
        indices-buffer (create-indices-buffer indices)]
    ; Bind VAO first
    (GL33/glBindVertexArray vao)

    ; Bind and fill VBO
    (GL33/glBindBuffer GL33/GL_ARRAY_BUFFER vbo)
    (GL33/glBufferData GL33/GL_ARRAY_BUFFER vertices-buffer GL33/GL_STATIC_DRAW)

    ; Bind and fill EBO
    (GL33/glBindBuffer GL33/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL33/glBufferData GL33/GL_ELEMENT_ARRAY_BUFFER indices-buffer GL33/GL_STATIC_DRAW)

    ; Position attribute (3 floats per vertex)
    (GL33/glVertexAttribPointer 0 3 GL33/GL_FLOAT false (int (* 6 Float/BYTES)) 0)
    (GL33/glEnableVertexAttribArray 0)

    ; Color attribute (3 floats per vertex)
    (GL33/glVertexAttribPointer 1 3 GL33/GL_FLOAT false (int (* 6 Float/BYTES)) (int (* 3 Float/BYTES)))
    (GL33/glEnableVertexAttribArray 1)

    (map->Mesh
      {:vao vao
       :vbo vbo
       :abo ebo
       :indices indices
       :model (.identity (Matrix4f.))
       :shader shader})))

(defrecord Line [vao vbo vertices model shader]
  IRenderable
  (render [this]
    (shader/set-uniform :m4f shader :model model)
    (GL33/glBindVertexArray vao)
    (GL33/glDrawArrays GL33/GL_LINE_STRIP 0 (count vertices)))

  ITransformable
  (rotation [this {:keys [x y z] :as rotation}]
    (rotation* model rotation))
  (rotate [this {:keys [x y z] :as rotation-dt}]
    (rotate* model rotation-dt)))
