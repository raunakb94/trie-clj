 (ns trie)

;; Basic DS of a node with a
;; 1. char val -> value of the node in the hierarchy
;; 2. child_nodes hash_map of subsequent level chars and respective trie_node
;; 3. is_terminal? flag to determine whether the particular level
;;    is actually the end of a string stored in the string. (used in fixed string search)
(defrecord Trie_node [^Character val child_nodes ^Integer level ^Boolean is_terminal? info])


(defn insert-node [^Trie_node parent ^Character val ^Boolean is_terminal? info]
  "Creates a new Trienode and stores it inside the required children oblects map of parent"
  (let [parent_level (:level parent)
        node (Trie_node. val {} (inc parent_level) is_terminal? info)
        child-nodes (-> (:child_nodes parent)
                        (assoc val node))]
    (assoc parent :child_nodes child-nodes)))

(defn get-child-node [^Trie_node parent ^Character val]
  "Returns child trie node of a particular parent"
  (-> parent :child_nodes (get val)))

(defn child-node-exists? [^Trie_node parent ^Character val]
  (if (get-child-node parent val)
    true
    false))

(defn store_trie [root string info]
  "Recursive method to store a string inside the dictionary maintained by a trie"
  (if (not-empty string)
    (let [current_level_char (char (first string))
          current_node (if (child-node-exists? root current_level_char)
                         root
                         (insert-node root (char (first string)) false nil))]
      (update-in current_node
                 [:child_nodes current_level_char]
                 #(store_trie % (rest string) info)))
    (assoc root :is_terminal? true :info info)))

(defn search_trie_node [root string]
  "Returns trie_node of the terminal matching the string pattern"
  (if (not-empty string)
    (let [current_level_char (char (first string))]
      (if (child-node-exists? root current_level_char)
        (search_trie_node (get-child-node root current_level_char)
                          (rest string))
        nil))
    root))

(defn search_trie [root string]
  "Boolean method to search substrings in a trie"
  (let [node (search_trie_node root string)]
    (if node
      true false)))

(defn search_trie_fixed [root string]
  "Boolean method to search complete strings in a trie"
  (let [node (search_trie_node root string)]
    (if (:is_terminal? node)
      true false)))

(defn traverse_trie_node
  "Traverses a trie root node and returns a vector containing
  full character sequences present inside the root node
  eg. if trie stores abc,abcd,abe it returns [[a,b,c] [a,b,c,d] [a b e]]"
  ([root]
   (traverse_trie_node root (vector)))
  ([root parent_chars]
   (let [current-chars (vec (conj parent_chars (:val root)))
         child-strings-seq (->> (for [[c node] (:child_nodes root)]
                                  (traverse_trie_node node current-chars))
                                (apply concat))]
     (if (:is_terminal? root)
       (conj child-strings-seq current-chars)
       child-strings-seq))))

(defn retrieve_trie_info
  "Returns level,info of all the traversal results"
  ([root]
   (let [current-info (:info root)
         level (:level root)
         child-info (->> (for [[c node] (:child_nodes root)]
                           (retrieve_trie_info node))
                         (apply concat))]
     (if (:is_terminal? root)
       (conj child-info {:info  current-info
                         :level level})
       child-info))))

(defn prefix_retrieve_trie_info [root prefix_string]
  "Returns level,info of all the traversal results which matches a prefix"
  (let [node (search_trie_node root prefix_string)
        level (:level node)
        child-nodes-info (->> (for [[_ n] (:child_nodes node)]
                                (retrieve_trie_info n))
                              (apply concat))]
    (if (:is_terminal? node)
      (conj child-nodes-info {:info (:info node)
                              :level level
                              :matched? true})
      child-nodes-info)))

(comment
  (def root (Trie_node. nil {} 0 false nil))
  (def r3 (-> (store_trie root "raunak" {:name "raunak"})
              (store_trie "rau" {:name "rau"})
              (store_trie "Aayush" {:name "Aayush"})))
  (insert-node root \b true)
  (def r2 (store_trie root "abcdef"))
  r2
  (def r3 (store_trie r3 "pqrstuv"))
  r3
  (search_trie_fixed r3 "abc")
  (print_trie_node r3 (prefix_traverse_node r3 "pqr"))
  )
