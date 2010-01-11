;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Проверка файловых путей."
       :author "Роман Захаров"}
  verified
  (:use :reload aux)
  (:use clojure.contrib.seq-utils)
  (:import java.io.File))

(in-ns 'verified)

(defmulti path
  "Проверяет существование пути и возврашает файловый объект."
  type)

(defmethod path nil [p] nil)

(defmethod path String [p]
  (let [pathfile (File. (.getCanonicalPath (File. p)))]
    (when (.exists pathfile) pathfile)))

(defmethod path File [p]
  (when (.exists p) p))

(defn jobs-file [p]
  (when-let [pathfile (path p)]
    (when (and (.isFile pathfile) (.canRead pathfile))
      pathfile)))

(defn log-file [p]
  (when (string? p)
    (let [file (File. (.getCanonicalPath (File. p)))
          dir (File. (.getParent file))]
      (when (and (.exists dir) (.canWrite dir))
        (when-not (.exists file) (.createNewFile file))
        file))))

(defn output-dir [p]
  (when-let [pathfile (path p)]
    (when (and (.isDirectory pathfile) (.canWrite pathfile))
      pathfile)))

(defn upload-file [p]
  (when-let [pathfile (path p)]
    (when (and (.isFile pathfile) (.canRead pathfile)
               (> (file-length pathfile) 0))
      pathfile)))

(defn upload-dir [p]
  (when-let [pathfile (path p)]
    (when (and (.isDirectory pathfile) (.canRead pathfile))
      pathfile)))

(defn upload-files [paths]
  (let [[dirs files]  (->> paths
                           (map path)
                           (remove nil?)
                           (split-with directory?))
        files-in-dirs (->> dirs
                           (map file-seq)
                           (flatten)
                           (remove directory?))]
    (->> (concat files files-in-dirs)
         (map upload-file)
         (remove nil?)
         (distinct)
         (sort))))