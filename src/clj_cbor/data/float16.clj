(ns clj-cbor.data.float16
  "Implementation of IEEE 754 half-precision floating point.")


(def zero              2r0000000000000000)
(def positive-infinity 2r0111110000000000)
(def negative-infinity 2r1111110000000000)
(def not-a-number      2r0111111000000000)


(defn from-bits
  "Returns a `float` value read as a half-precision IEEE floating-point number
  from the lower two bytes of x."
  [x]
  (let [combine (fn combine-float
                  [sign exp mant]
                  (Float/intBitsToFloat
                    (bit-or (if (zero? sign) 0 Integer/MIN_VALUE)
                            (bit-shift-left (bit-or exp mant) 13))))
        sign (bit-and x 0x8000)
        exp  (bit-and x 0x7c00)
        mant (bit-and x 0x03ff)]
    (cond
      ; NaN and Infinite values.
      (= exp 0x7c00)
        (combine sign 0x3fc00 mant)

      ; Normalized value.
      (not (zero? exp))
        (combine sign (+ exp 0x1c000) mant)

      ; Subnormal value.
      (not (zero? mant))
        (loop [exp 0x1c400
               mant mant]
          (if (zero? (bit-and mant 0x400))
            (recur (- exp 0x400) (bit-shift-left mant 1))
            (combine sign exp (bit-and mant 0x3ff))))

      ; +/- 0
      :else
        (combine sign exp mant))))


(defn to-bits
  "Returns an integer whose lower two bytes encode the given number in the
  half-precision IEEE floating point format."
  [x]
  (let [fbits (Float/floatToIntBits (float x))
        sign (bit-and (unsigned-bit-shift-right fbits 16)
                      0x8000)
        value (+ (bit-and fbits 0x7fffffff) 0x1000)]     ; rounded value
    (cond
      ; Value might be or become NaN/Inf.
      (>= value 0x47800000)
        (if (< value 0x7f800000)
          ; Value was too large, promote to infinity.
          (bit-or sign 0x7c00)
          ; Value remains NaN or +/-Inf.
          (bit-or sign 0x7c00 (unsigned-bit-shift-right
                                (bit-and fbits 0x007fffff)
                                13)))

      ; Retain normalized value.
      (>= value 0x38800000)
        (bit-or sign (unsigned-bit-shift-right (- value 0x38000000) 13))

      ; Value is too small, becomes +/-0
      (< value 0x33000000)
        sign

      ; Fix subnormal value.
      :else
        (let [exp (unsigned-bit-shift-right (bit-and fbits 0x7fffffff) 23)]
          (bit-or sign
                  (unsigned-bit-shift-right
                    (+ (bit-or (bit-and fbits 0x7fffff)
                               0x800000)
                       (unsigned-bit-shift-right 0x800000 (- exp 102)))
                    (- 126 exp)))))))
