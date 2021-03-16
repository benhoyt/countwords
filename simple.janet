#!/usr/bin/env janet

(var counts @{})

(loop [line :iterate (file/read stdin :line)]
  (def lowered (string/ascii-lower line))
  (def words (map string/trim (string/split " " lowered)))
  (loop [word :in words]
    (if (> (length word) 0)
      (update-in counts [word] (fn [x] (+ 1 (or x 0)))))))

(def words (keys counts))
(sort words
  (fn [a b] (> (counts a) (counts b))))

(loop [word :in words]
  (def freq (get counts word))
  (print word " " freq))

(os/exit 0)
