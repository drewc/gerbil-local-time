;;; -*- Gerbil -*-
(export #t)
(import
   :std/misc/bytes
   :std/srfi/13
   :clan/base)

(defclass tzfile
  (magic version header
   transition-times transition-time-index
   ttinfo-structures
   timezone-abbreviation-array
   leap-second-records
   wall-array
   utc-array))

(def (tzread port byte-count (function identity))
    (let (u8vector (make-u8vector byte-count))
      (read-u8vector u8vector port)
      (function u8vector)))

(def (read-u32 p)
  (tzread p 4 u8vector->nat))
(def (read-s32 p)
  (tzread p 4 u8vector->integer))

(defstruct ttinfo
  (gmt-offset is-dst abbreviation-index))

(def (tzread-ttinfo p)
  (let* ((g (read-s32 p))
         (d (read-u8 p))
         (a (read-u8 p)))
    (make-ttinfo g d a)))

(def (tzread-abbreviations p abbreviation-length)
  (def (make-abbrev-array string)
    (nest
     (list->vector)
     (let buf ((b string)))
     (if (zero? (string-length b)) '())
     (let ((index (string-index b #\nul))))
     (if (not index) '()
         (cons (substring/shared  b 0 index)
               (buf (substring/shared b (+ 1 index)))))))
  (make-abbrev-array
   (tzread p abbreviation-length bytes->string)))

(def (read-tzfile tzfile)
  (call-with-input-file tzfile
    (λ (p)
      (let* (
             ;; The magic four-byte sequence "TZif" identifying this as a timezone
             ;; information file.
             (magic (tzread p 4 bytes->string))

             ;; A single character identifying the version of the
             ;; file's format:
             ;; either an ASCII NUL ('\0') or a '2' (0x32).
             (version (tzread p 1 bytes->string))

             ;; Fifteen bytes containing zeros reserved for future use.
             (future (tzread p 15))

             ;; Six four-byte values of type long, written in a "standard"
             ;; byte order (the high-order byte of the value is written
             ;; first).  These values are, in order:

             ;; tzh_ttisgmtcnt
             ;;    The number of UTC/local indicators stored in the file.
             (utc-count (read-s32 p))

             ;; tzh_ttisstdcnt
             ;;        The number of standard/wall indicators stored in the file.
             (wall-count (read-s32 p))


             ;; tzh_leapcnt
             ;;  The number of leap seconds for which data is  stored  in  the
             ;;  file.
             (leap-count (read-s32 p))

             ;; tzh_timecnt

             ;; The number of "transition times" for which
             ;;  data is stored in the file.
             (transition-count (read-s32 p))

             ;; tzh_typecnt

             ;; The number of "local time types" for which data is stored
             ;; in the file (must not be zero).

             (type-count (read-s32 p))

             ;; tzh_charcnt
             ;; The  number  of characters of "timezone abbreviation strings"
             ;; stored in the file.

             (abbreviation-length (read-s32 p))


             ;; The above header is followed by tzh_timecnt four-byte
             ;; values of type long, sorted in ascending order.

             (transition-times
              (let loop ((times transition-count))
                (if (zero? times) '()
                    (cons (read-s32 p)
                          (loop (- times 1))))))

             ;; Next come tzh_timecnt one-byte values of type unsigned
             ;; char.

             (transition-indices
              (let loop ((times transition-count))
                (if (zero? times) '()
                    (cons (read-u8 p)
                          (loop (- times 1))))))

             ;; These values serve as indices into an array of ttinfo
             ;; structures  with tzh_typecnt entries

             (info-structures
              (let loop ((times type-count))
                (cons (tzread-ttinfo p)
                      (if (= 1 times) '()
                          (loop (- times 1))))))


             ;; Then there are tzh_leapcnt pairs of four-byte values,
             ;; written in standard byte order; the first value of each
             ;; pair gives the time (as returned by time(2)) at which a
             ;; leap second occurs; the second gives the total number of
             ;; leap seconds to be applied after the given time.
             (leap-seconds
              (let loop ((times leap-count))
                (if (zero? times) '()
                    (cons (cons (read-u32 p)
                                (read-u32 p))
                          (loop (- times 1))))))

             ;; array of timezone  abbreviation  characters
             (abbreviation-array
              (tzread-abbreviations p abbreviation-length))

        ;; there are tzh_ttisstdcnt standard/wall indicators, each stored as
        ;; a one-byte value
        (wall-indicators
         (tzread p wall-count))

        ;;  Finally, there are tzh_ttisgmtcnt UTC/local indicators,
        ;; each stored as a one-byte value

        (utc-indicators
         (tzread p utc-count)))

      (make-tzfile
       magic: magic version: version
       header: (list future: future
                     utc-count: utc-count
                     wall-count: wall-count
                     leap-count: leap-count
                     transition-count: transition-count
                     type-count: type-count
                     abbreviation-length: abbreviation-length)
       transition-times: transition-times
       transition-time-index: transition-indices
       ttinfo-structures: info-structures
       timezone-abbreviation-array: abbreviation-array
       leap-second-records: leap-seconds
       wall-array: wall-indicators
       utc-array: utc-indicators)))))
