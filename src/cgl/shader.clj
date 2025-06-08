(ns cgl.shader
  (:require [cgl.camera :as camera]
            [cgl.window :as win])
  (:import (java.nio FloatBuffer)
           (org.joml Matrix4f)
           (org.lwjgl BufferUtils)
           (org.lwjgl.opengl GL33)))

(defn create-shader [type ^String source]
  (let [shader (GL33/glCreateShader ^int type)]
    (GL33/glShaderSource shader source)
    (GL33/glCompileShader shader)

    (when (zero? (GL33/glGetShaderi shader GL33/GL_COMPILE_STATUS))
      (let [log (GL33/glGetShaderInfoLog shader)]
        (GL33/glDeleteShader shader)
        (throw (RuntimeException. (str "Shader compilation failed: " log)))))

    shader))

(defn set-uniform [type program uniform value]
  (let [{:keys [location buffer]} (-> program :uniforms uniform)]
    (case type
      :m4f (do
             (.get ^Matrix4f value ^FloatBuffer buffer)
             (GL33/glUniformMatrix4fv ^int location false ^FloatBuffer buffer)))))

(defn update-view-matrix [camera shader-program]
  (let [view (camera/get-view-matrix camera)]
    (set-uniform :m4f shader-program :view view)))

(defn update-projection-matrix [shader-program]
  (let [aspect-ratio (/ (float @win/window-width*) (float @win/window-height*))
        projection (-> (Matrix4f.)
                       (.identity)
                       (.perspective (Math/toRadians 45.0) aspect-ratio 0.1 100.0))]
    (set-uniform :m4f shader-program :projection projection)))

(defprotocol IShaderProgram
  (set-uniforms [this inputs]))

(defrecord ShaderProgram [program uniforms]
  IShaderProgram
  (set-uniforms [this {:keys [camera]}]
    (update-view-matrix camera this)
    (update-projection-matrix this)))

(defn create-shader-program [vertex-shader-source fragment-shader-source]
  (let [vertex-shader (create-shader GL33/GL_VERTEX_SHADER vertex-shader-source)
        fragment-shader (create-shader GL33/GL_FRAGMENT_SHADER fragment-shader-source)
        program (GL33/glCreateProgram)]

    (GL33/glAttachShader program vertex-shader)
    (GL33/glAttachShader program fragment-shader)
    (GL33/glLinkProgram program)

    (when (zero? (GL33/glGetProgrami program GL33/GL_LINK_STATUS))
      (let [log (GL33/glGetProgramInfoLog program)]
        (throw (RuntimeException. (str "Program linking failed: " log)))))

    (GL33/glDeleteShader vertex-shader)
    (GL33/glDeleteShader fragment-shader)

    (GL33/glUseProgram program)

    (ShaderProgram.
      program
      {:model {:location (GL33/glGetUniformLocation program "model")
               :buffer (BufferUtils/createFloatBuffer 16)}
       :view {:location (GL33/glGetUniformLocation program "view")
              :buffer (BufferUtils/createFloatBuffer 16)}
       :projection {:location (GL33/glGetUniformLocation program "projection")
                    :buffer   (BufferUtils/createFloatBuffer 16)}})))
