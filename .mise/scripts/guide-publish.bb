;; Publish the guide: master from the working tree, and each minor version from git.
;;
;; A minor version's content (src/, examples/) comes from its latest patch tag, or from the override
;; in docs/guide/versions.edn. Its theme (book.toml, CSS, theme/) comes from the working tree, so
;; that all versions look the same. Versions outside the window stay as they were last built.
(require '[babashka.fs :as fs]
         '[babashka.process :refer [shell]]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

;; Older minor versions are rarely read, so stop rebuilding them.
(def window 10)
;; Older tags have guide sources, but their guide was never published.
(def first-version [0 6])
(def url-path "/emacs-module-rs")
(def repo-url "https://github.com/ubolonton/emacs-module-rs")
;; The guide moved from guide/ to docs/guide/ in 0.23.
(def guide-dirs ["docs/guide" "guide"])
(def content-dirs ["src" "examples"])
(def theme-files ["book.toml" "custom.css" "theme"])

(def root (fs/path (System/getenv "MISE_PROJECT_ROOT")))
(def guide (fs/path root "docs" "guide"))
(def site-dir (fs/absolutize (System/getenv "usage_site_dir")))
(def build-all? (= "true" (System/getenv "usage_all")))

(defn git [& args]
  (:out (apply shell {:dir (str root) :out :string} "git" args)))

(defn tree-has? [ref path]
  (not (str/blank? (git "ls-tree" "--name-only" ref path))))

(defn guide-dir
  "Where the guide lives at `ref`, if it has one."
  [ref]
  (first (filter #(tree-has? ref (str % "/src")) guide-dirs)))

(defn parse-version [s]
  (some->> (re-matches #"(\d+)\.(\d+)\.(\d+)" s) rest (mapv parse-long)))

(defn minor-versions
  "[minor latest-patch-tag] pairs, newest first."
  []
  (->> (str/split-lines (git "tag" "--list"))
       (keep parse-version)
       (group-by #(subvec % 0 2))
       (filter #(>= (compare (key %) first-version) 0))
       (sort-by key #(compare %2 %1))
       (map (fn [[minor versions]]
              [(str/join "." minor) (str/join "." (last (sort versions)))]))))

(defn mdbook-build [book name & [edit-url]]
  ;; mdBook empties the destination first, so no stale files stay.
  (shell {:extra-env (cond-> {"MDBOOK_BOOK__TITLE" (str "emacs-module-rs " name)
                              "MDBOOK_OUTPUT__HTML__SITE_URL" (str url-path "/" name "/")}
                       edit-url (assoc "MDBOOK_OUTPUT__HTML__EDIT_URL_TEMPLATE" edit-url))}
         "mdbook" "build" (str book) "--dest-dir" (str (fs/path site-dir name))))

(defn build-version [name ref]
  (if-let [source (guide-dir ref)]
    (let [book (fs/path root "target" "guide-publish" name)
          archive (fs/path root "target" "guide-publish" (str name ".tar"))
          content (fs/path book source)]
      (fs/delete-tree book)
      (fs/create-dirs book)
      (apply git "archive" "--output" (str archive) ref
             (filter #(tree-has? ref %) (map #(str source "/" %) content-dirs)))
      (shell "tar" "-xf" (str archive) "-C" (str book))
      (doseq [path theme-files]
        (if (fs/directory? (fs/path guide path))
          (fs/copy-tree (fs/path guide path) (fs/path content path))
          (fs/copy (fs/path guide path) (fs/path content path))))
      (println (str name ": building from " ref))
      ;; Released guides can't be edited, so link to their source.
      (mdbook-build content name (str repo-url "/blob/" ref "/" source "/{path}")))
    (println (str name ": " ref " has no guide, skipped"))))

(defn write-versions
  "Write versions.json for version-switcher.js: master, then minor versions, newest first."
  []
  (let [minors (->> (fs/list-dir site-dir)
                    (filter fs/directory?)
                    (map fs/file-name)
                    (filter #(re-matches #"\d+\.\d+" %))
                    (sort-by #(mapv parse-long (str/split % #"\.")) #(compare %2 %1)))
        names (cond->> minors (fs/directory? (fs/path site-dir "master")) (cons "master"))]
    (spit (str (fs/path site-dir "versions.json"))
          (json/generate-string
           (for [name names]
             {:name name
              :pages (sort (map fs/file-name (fs/glob (fs/path site-dir name) "*.html")))})
           {:pretty true}))))

(fs/create-dirs site-dir)
(mdbook-build guide "master")

(let [minors (filter (comp guide-dir second) (minor-versions))
      overrides (edn/read-string (slurp (str (fs/path guide "versions.edn"))))]
  (doseq [[minor tag] (if build-all? minors (take window minors))]
    (build-version minor (get overrides tag tag)))
  (let [latest (fs/path site-dir "latest")]
    (fs/delete-if-exists latest)
    (fs/create-sym-link latest (first (first minors)))))

;; All versions load the menu from the site root, so versions outside the window get fixes too.
(fs/copy (fs/path guide "version-switcher.js") (fs/path site-dir "version-switcher.js")
         {:replace-existing true})
(write-versions)
