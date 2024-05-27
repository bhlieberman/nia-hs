(ns com.bhlie.nia.v2.data)

(def decoder (js/TextDecoder.))

(def buffer (atom ""))

(declare append-chunks read-chunks)

(defn read-chunks [reader] 
  (try (.. reader read (then (fn [chunk] (append-chunks chunk reader))))
       (catch js/TypeError _
         (println reader))))

(defn append-chunks [chunk rdr]
  (js/console.log (.-done chunk))
  (let [c (.decode decoder (.-value chunk) #js {:stream (not (.-done chunk))})]
    (js/console.log "got chunk of" (.-length c) "bytes")
    (swap! buffer str c)
    (if (.-done chunk)
      @buffer
      (read-chunks rdr))))

(defn process-chunk [^js resp]
  (let [^js reader (.. resp -body getReader)]
    (.. reader read (then (fn [chunk] (append-chunks chunk reader))))))

(comment
  (-> (js/fetch "http://localhost:3000/streamed")
      (.then process-chunk)))