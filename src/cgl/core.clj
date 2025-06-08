(ns cgl.core
  (:require [nrepl.server]
            [cgl.camera :as camera]
            [cgl.geometry :as geo]
            [cgl.window :as win]
            [cgl.shader :as shader]
            [cgl.mesh :as mesh])
  (:import [java.nio FloatBuffer IntBuffer]
           [org.lwjgl.glfw GLFW]
           [org.lwjgl.opengl GL GL33]
           [org.lwjgl BufferUtils]
           [org.joml Matrix4f]))

(def vertex-shader-source
  "#version 330 core
   layout (location = 0) in vec3 aPos;
   layout (location = 1) in vec3 aColor;
   out vec3 ourColor;
   uniform mat4 model;
   uniform mat4 view;
   uniform mat4 projection;
   void main() {
     gl_Position = projection * view * model * vec4(aPos, 1.0);
     ourColor = aColor;
   }")

(def fragment-shader-source
  "#version 330 core
   in vec3 ourColor;
   out vec4 FragColor;
   void main() {
     FragColor = vec4(ourColor, 1.0);
   }")

(def camera* (atom nil))
(def shader-program* (atom nil))

(def objects* (atom {}))


(defn create-cube []
  (let [vertices (float-array [; positions        ; colors
                               -0.5 -0.5 -0.5 1.0 0.0 0.0 ; front bottom left
                               0.5 -0.5 -0.5 0.0 1.0 0.0 ; front bottom right
                               0.5 0.5 -0.5 0.0 0.0 1.0 ; front top right
                               -0.5 0.5 -0.5 1.0 1.0 0.0 ; front top left
                               -0.5 -0.5 0.5 1.0 0.0 1.0 ; back bottom left
                               0.5 -0.5 0.5 0.0 1.0 1.0 ; back bottom right
                               0.5 0.5 0.5 0.7 0.7 0.7 ; back top right
                               -0.5 0.5 0.5 1.0 1.0 1.0]) ; back top left

        indices (int-array [; front face (z = -0.5)
                            0 1 2,
                            2 3 0,
                            ; back face (z = 0.5)
                            4 5 6,
                            6 7 4,
                            ; left face (x = -0.5)
                            0 3 7,
                            7 4 0,
                            ; right face (x = 0.5)
                            1 5 6,
                            6 2 1,
                            ; bottom face (y = -0.5)
                            0 4 5,
                            5 1 0,
                            ; top face (y = 0.5)
                            3 2 6,
                            6 7 3])]

    (mesh/create-mesh
      {:vertices vertices
       :indices indices
       :shader @shader-program*})))

(defn create-curve-path [num-segments radius-x radius-y]
  ; Create an infinity-shaped path
  (let [path (geo/create-infinity-path num-segments radius-x radius-y)]

    ; Create a line visualization for the path
    (let [vertices (float-array (apply concat
                                       (for [point path]
                                         [(.x point) (.y point) (.z point)
                                          1.0 1.0 1.0]))) ; White color for path
          vao (GL33/glGenVertexArrays)
          vbo (GL33/glGenBuffers)]

      (GL33/glBindVertexArray vao)
      (GL33/glBindBuffer GL33/GL_ARRAY_BUFFER vbo)

      (let [buffer (-> (BufferUtils/createFloatBuffer (count vertices))
                       (.put vertices)
                       (.flip))]
        (GL33/glBufferData GL33/GL_ARRAY_BUFFER buffer GL33/GL_STATIC_DRAW))

      ; Position attribute (3 floats)
      (GL33/glVertexAttribPointer 0 3 GL33/GL_FLOAT false (int (* 6 Float/BYTES)) 0)
      (GL33/glEnableVertexAttribArray 0)

      ; Color attribute (3 floats)
      (GL33/glVertexAttribPointer 1 3 GL33/GL_FLOAT false (int (* 6 Float/BYTES)) (int (* 3 Float/BYTES)))
      (GL33/glEnableVertexAttribArray 1)

      (mesh/map->Line
        {:vao vao
         :vbo vbo
         :vertices path
         :model (.identity (Matrix4f.))
         :shader @shader-program*}))))

(defn create-tube [path]
  (let [; Create tube along the path
        tube-radius 0.15
        circle-segments 16

        ; Make the path complete a full loop by adding the first point again
        ; This ensures we don't have gaps at the beginning/end
        closed-path (conj (vec path) (first path))

        tube-data (geo/create-tube closed-path tube-radius circle-segments)

        ; Convert tube data to vertex arrays for OpenGL
        vertex-data (geo/tube-to-vertex-data tube-data)
        vertices (:vertex-array vertex-data)
        indices (:index-array vertex-data)

        ; Create a color array - we'll make a rainbow effect along the tube
        color-count (/ (count vertices) 6) ; 6 floats per vertex (position + normal)
        colors (float-array
                (apply concat
                       (for [i (range color-count)]
                         (let [t (/ i (float color-count))
                               ; Rainbow colors
                               r (-> (Math/sin (* t Math/PI 2.0)) (+ 1.0) (/ 2.0))
                               g (-> (Math/sin (+ (* t Math/PI 2.0) (/ Math/PI 3.0))) (+ 1.0) (/ 2.0))
                               b (-> (Math/sin (+ (* t Math/PI 2.0) (/ (* 2.0 Math/PI) 3.0))) (+ 1.0) (/ 2.0))]
                           [r g b]))))

        ; Create buffers
        vao (GL33/glGenVertexArrays)
        vbo-positions (GL33/glGenBuffers)
        vbo-colors (GL33/glGenBuffers)
        ebo (GL33/glGenBuffers)

        ; Create and fill vertex buffer
        vertices-buffer (-> (BufferUtils/createFloatBuffer (count vertices))
                            (.put vertices)
                            (.flip))

        ; Create and fill colors buffer
        colors-buffer (-> (BufferUtils/createFloatBuffer (count colors))
                          (.put colors)
                          (.flip))

        ; Create and fill element buffer
        indices-buffer ^IntBuffer (-> (BufferUtils/createIntBuffer (count indices))
                                      (.put indices)
                                      (.flip))]

    ; Bind VAO
    (GL33/glBindVertexArray vao)

    ; Bind positions VBO
    (GL33/glBindBuffer GL33/GL_ARRAY_BUFFER vbo-positions)
    (GL33/glBufferData GL33/GL_ARRAY_BUFFER ^FloatBuffer vertices-buffer GL33/GL_STATIC_DRAW)

    ; Position attribute (3 floats)
    (GL33/glVertexAttribPointer 0 3 GL33/GL_FLOAT false (int (* 6 Float/BYTES)) 0)
    (GL33/glEnableVertexAttribArray 0)

    ; Bind colors VBO
    (GL33/glBindBuffer GL33/GL_ARRAY_BUFFER vbo-colors)
    (GL33/glBufferData GL33/GL_ARRAY_BUFFER colors-buffer GL33/GL_STATIC_DRAW)

    ; Color attribute (3 floats)
    (GL33/glVertexAttribPointer 1 3 GL33/GL_FLOAT false (int (* 3 Float/BYTES)) 0)
    (GL33/glEnableVertexAttribArray 1)

    ; Bind EBO
    (GL33/glBindBuffer GL33/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL33/glBufferData GL33/GL_ELEMENT_ARRAY_BUFFER indices-buffer GL33/GL_STATIC_DRAW)

    (mesh/map->Mesh
      {:vao vao
       :vbo vbo-positions
       :ebo ebo
       :indices indices
       :model (.identity (Matrix4f.))
       :shader @shader-program*})))

(defn init-opengl []
  (GL/createCapabilities)

  (println "OpenGL Vendor: " (GL33/glGetString GL33/GL_VENDOR))
  (println "OpenGL Renderer: " (GL33/glGetString GL33/GL_RENDERER))
  (println "OpenGL Version: " (GL33/glGetString GL33/GL_VERSION))
  (println "GLSL Version: " (GL33/glGetString GL33/GL_SHADING_LANGUAGE_VERSION))

  (GL33/glEnable GL33/GL_DEPTH_TEST)

  (win/update-viewport-and-projection)

  (reset! shader-program* (shader/create-shader-program
                            vertex-shader-source
                            fragment-shader-source))

  (reset! camera* (camera/create-camera))

  (swap! objects* assoc :path (create-curve-path 100 1.5 1.5))
  (swap! objects* assoc :tube (create-tube (:vertices (:path @objects*))))
  (swap! objects* assoc :cube1 (create-cube)))

(defn process-input [])

(defn render-loop []
  (let [dt (win/step)]
    (GL33/glClearColor 0.1 0.1 0.2 1.0)
    (GL33/glClear (bit-or GL33/GL_COLOR_BUFFER_BIT GL33/GL_DEPTH_BUFFER_BIT))

    (GL33/glUseProgram (:program @shader-program*))
    (shader/set-uniforms @shader-program* {:camera @camera*})

    (mesh/rotation (:tube @objects*) {:x (Math/cos @win/current-time*)})
    (mesh/rotate (:cube1 @objects*) {:x dt})

    (doseq [[_ obj] @objects*]
      (mesh/render obj))))

(defn run []
  (win/init)
  (init-opengl)

  (while (not (GLFW/glfwWindowShouldClose @win/window*))
    (process-input)
    (render-loop)
    (GLFW/glfwSwapBuffers @win/window*)
    (GLFW/glfwPollEvents))

  (win/teardown))

(defn -main [& [repl?]]
  (when repl?
    (nrepl.server/start-server :port 7888)
    (println "nrepl server started on port 7888"))
  (run))
