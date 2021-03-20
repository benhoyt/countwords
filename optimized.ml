module Buf = struct
  type t = { buf : Bytes.t; mutable pos : int; mutable len : int }

  let create size = { buf = Bytes.create size; pos = 0; len = 0 }

  let compress t =
    if t.len <= 0 || t.pos = t.len then (
      t.pos <- 0;
      t.len <- 0)
    else if t.pos > 0 then (
      (* Shifting the unconsumed bytes to the start of the buffer. We will always
         have enough room to shift these bytes so it should be fine to use unsafe_blit here. *)
      BytesLabels.unsafe_blit ~src:t.buf ~src_pos:t.pos ~dst:t.buf ~dst_pos:0
        ~len:t.len;
      t.pos <- 0)

  let read_into t fd =
    compress t;
    let count =
      UnixLabels.read fd ~buf:t.buf ~pos:(t.pos + t.len) ~len:(Bytes.length t.buf - t.len)
    in
    t.len <- t.len + count;
    for i = t.pos to t.len - 1 do
      Bytes.unsafe_set t.buf i (Char.lowercase_ascii (Bytes.unsafe_get t.buf i))
    done;
    count

  let[@inline] is_delim c = c = '\n' || c = ' '

  let iter_words f t =
    let prev = ref t.pos in
    for i = t.pos to t.len - 1 do
      (* We perform bounds check so its okay to call unsafe_get here *)
      if is_delim (Bytes.unsafe_get t.buf i) then (
        f t.buf !prev (i - !prev);
        prev := i + 1)
    done;
    t.len <- t.len - !prev;
    t.pos <- !prev
end

module BytesHash = struct
  include Bytes

  let hash (k : bytes) = Hashtbl.hash k
end

module BytesHashtbl = Hashtbl.Make (BytesHash)

let count_word countwords buf pos len =
  if len > 0 then
    let word = Bytes.sub buf pos len in
    try incr (BytesHashtbl.find countwords word)
    with Not_found -> BytesHashtbl.add countwords word (ref 1)

let () =
  let countwords = BytesHashtbl.create 33_000 in
  let buf = Buf.create (64 * 1024) in
  while Buf.read_into buf Unix.stdin > 0 do
    Buf.iter_words (fun buf pos len -> count_word countwords buf pos len) buf
  done;
  let arr = Array.make (BytesHashtbl.length countwords) (Bytes.empty, 0) in
  let idx = ref 0 in
  BytesHashtbl.iter
    (fun word count ->
      arr.(!idx) <- (word, !count);
      incr idx)
    countwords;
  Array.sort (fun (_, x) (_, y) -> Int.compare y x) arr;
  Array.iter
    (fun (w, c) ->
      print_bytes w;
      print_char ' ';
      print_int c;
      print_newline ())
    arr
