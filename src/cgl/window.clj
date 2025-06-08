(ns cgl.window
  (:import [org.lwjgl.glfw Callbacks GLFW GLFWErrorCallback]
           [org.lwjgl.opengl GL GL33]
           [org.lwjgl.system MemoryUtil]
           [org.lwjgl BufferUtils]))

;; ================= TIME ====================

(def current-time* (volatile! (GLFW/glfwGetTime)))

(defn step []
  (let [current-time (GLFW/glfwGetTime)
        dt (- current-time @current-time*)
        _ (vreset! current-time* current-time)]
    dt))

;; ================= /TIME ====================

(def window* (atom nil))

(def keys-pressed* (atom {}))
(def window-width* (atom 640))
(def window-height* (atom 480))

(defn update-viewport-and-projection []
  ; On macOS with Retina displays, the framebuffer size may differ from the window size
  ; We need to get the actual framebuffer size for the viewport
  (let [width-buffer (BufferUtils/createIntBuffer 1)
        height-buffer (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetFramebufferSize (long @window*) width-buffer height-buffer)
    (let [framebuffer-width (.get width-buffer 0)
          framebuffer-height (.get height-buffer 0)]
      ; Reset the atoms to the actual framebuffer size
      (reset! window-width* framebuffer-width)
      (reset! window-height* framebuffer-height)
      ; Update the viewport with the framebuffer size
      (GL33/glViewport 0 0 framebuffer-width framebuffer-height))))

(defn init-window []
  (.set (GLFWErrorCallback/createPrint System/err))

  (when-not (GLFW/glfwInit)
    (throw (IllegalStateException. "Unable to initialize GLFW")))

  (GLFW/glfwDefaultWindowHints)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL33/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_FLOATING 1)

  (let [window (GLFW/glfwCreateWindow (int @window-width*) (int @window-height*) "OpenGL 3.3 Window" MemoryUtil/NULL MemoryUtil/NULL)]
    (when (= window MemoryUtil/NULL)
      (GLFW/glfwTerminate)
      (throw (RuntimeException. "Failed to create the GLFW window")))

    ; Set framebuffer size callback
    (GLFW/glfwSetFramebufferSizeCallback window
                                         (fn [window width height]
                                           (reset! window-width* width)
                                           (reset! window-height* height)
                                           (update-viewport-and-projection)))

    (reset! window* window)))

(defn init-inputs []
  ; Key callback - for single press events (like escape to exit)
  (GLFW/glfwSetKeyCallback @window*
                           (fn [window key scancode action mods]
                             (cond
                               ; Exit on escape key release
                               (and (= key GLFW/GLFW_KEY_ESCAPE)
                                    (= action GLFW/GLFW_RELEASE))
                               (GLFW/glfwSetWindowShouldClose window true)

                               ; Track key press state for continuous movement
                               (= action GLFW/GLFW_PRESS)
                               (swap! keys-pressed* assoc key true)

                               (= action GLFW/GLFW_RELEASE)
                               (swap! keys-pressed* dissoc key)))))

(defn init-context []
  (GLFW/glfwMakeContextCurrent @window*)
  (GLFW/glfwSwapInterval 1))

(defn init []
  (init-window)
  (init-inputs)
  (init-context))

(defn teardown []
  (Callbacks/glfwFreeCallbacks @window*)
  (GLFW/glfwDestroyWindow @window*)
  (GLFW/glfwTerminate)
  (.free (GLFW/glfwSetErrorCallback nil)))
