(ns blush.atproto-tid)

(defn pad-start [s len char]
  (let [str-length (count s)]
   (if (>= str-length len) s
       (let [pad (apply str (repeat (- len str-length) char))]
         (str pad s)))))

(def S32-CHAR "234567abcdefghijklmnopqrstuvwxyz")

(defn s32-encode [n]
  (loop [s "" n n]
    (if (zero? n)  s
      (let [c (mod n 32)]
        (recur (str (get S32-CHAR c) s)
               (quot n 32))))))

(defn s32-decode [s]
  (loop [i 0
         chars s]
    (if (not chars)  i
        (recur (+ (* i 32) (.indexOf S32-CHAR (str (first chars))))
               (next chars)))))


(def TID-RE #"^[234567abcdefghij][234567abcdefghijklmnopqrstuvwxyz]{12}$")

(def last-timestamp (atom 0))

(defn create-raw [timestamp clockid]
  (str (pad-start (s32-encode timestamp) 11 \2)
       (pad-start (s32-encode clockid)    2 \2)))

(defn now []
  (let [id (rand-int 1023)
        timestamp (max (* (.getTime (java.util.Date.)) 1000) @last-timestamp)]
    (when (= timestamp @last-timestamp)
      (swap! last-timestamp inc))
    (reset! last-timestamp timestamp)
    (create-raw timestamp id)))

(defn create [timestamp clockid]
  (cond
    (or (< timestamp 0) (not (integer? timestamp))) (throw (Exception. "Invalid timestamp"))
    (or (< clockid 0)   (> clockid 1024))           (throw (Exception. "Invalid clockid"))
    :else (create-raw timestamp clockid)))

(defn parse [tid]
  (cond
    (not= (count tid) 13)        (throw (Exception. "Incorrect TID length"))
    (not (re-find TID-RE tid))   (throw (Exception. "Invalid TID"))
    :else {:timestamp (s32-decode (subs tid 0 11))
           :clockid   (s32-decode (subs tid 11 13))}))

