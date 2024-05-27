; hikmet can koseoglu
; 2021400216
; compiling: yes
; complete: yes
#lang racket


; 1- binary_to_decimal

(define (binary_to_decimal binary)
  (let ((length (string-length binary)))
    (let loop ((index (- length 1))
               (result 0))
      (if (< index 0)
          result
          (let ((current-bit (string-ref binary index)))
            (if (char=? current-bit #\0)
                (loop (- index 1) result)
                (loop (- index 1) (+ result (expt 2 (- length index 1))))))))))

; 2- relocator
; (relocator '("000010100111" "010000110001" "100100111101" "100110010001" "101111011000") 3500 1200 )

(define (relocator args limit base)
  (let ((map_func (lambda (x) (let ((decimal (binary_to_decimal x))) (if (> decimal limit) -1 (+ base decimal)) ))))
    (map map_func args)))

; 3- divide_address_space
; (divide_address_space "11011011011000" 4)

; function log2
(define (log2 x)
  (if (= x 1) 0 (+ 1 (log2 (/ x 2)))))

; Function for splitting string into two pieces
(define (split_string_at_index str index)
  (list (substring str 0 index) 
        (substring str index)))

(define (divide_address_space num page_size)
  (split_string_at_index num (- (string-length num) (+ 10 (log2 page_size)))))

; 4- page
;  (page '("110010111011001" "000001111111010" "010001100000100" "101001011011101") '( "100" "000" "010" "110" "011" "001" "111" "101") 4 )

; function for accessing list element at index
(define (get_element lst index)
  (cond ((null? lst) '())                 
        ((= index 0) (car lst))            
        (else (get_element (cdr lst) (- index 1)))))

(define (page args page_table page_size)
  (let ((map_func (lambda (x)
                    (let ( (last_bits (+ 10 (log2 page_size)))
                           (str_len (string-length x)))
                      (let ((split_index (- str_len last_bits)))
                        (let ((first_part (get_element page_table (binary_to_decimal (substring x 0 split_index))))
                              (second_part (substring x split_index)))
                          (string-append first_part second_part))))
                  )))
    (map map_func args)))


; 5- find_sin
(define (find_sin value num)
  (define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))

  (define (term n x)
    (/ (expt x (+ 1 (* 2 n))) (factorial (+ 1 (* 2 n)))))

  (define (sine-recursive x n)
    (if (= n 0)
        0
        (+ (* (if (odd? n) 1 -1) (term (- n 1) x))
           (sine-recursive x (- n 1)))))

  (let ((radians (/ (* value pi) 180)))
    (sine-recursive radians num)))


; 6- myhash

; function that gets the first digit of floating part
(define (first_floating x)
  (let ((fractional-str (number->string (- x (floor x)))))
    (let ((first-digit (substring fractional-str 2 3)))
      (string->number first-digit))))

;function for summing 10 digits after decimal
(define (sum_ten float)
  (define (helper num remaining)
    (if (= remaining 0) 0 (+ (first_floating num) (helper (* num 10) (- remaining 1)))))
  (helper float 10))

(define (myhash arg table_size)
  (let ((decimal (binary_to_decimal arg)))
    (let ((sin_val (find_sin decimal (+ 1 (modulo decimal 5)) )))
      (modulo (sum_ten sin_val) table_size) ) ) )


; 7- hashed_page
; (hashed_page "010010111111101" 3 '( ( ("01" "000") ) ( ("11" "010") ) ( ("10" "111")) ) 8)

; function for matching list head with given value recursively and return tail value
(define (match_head lst elem)
  (if (null? lst) '()
      (let ((checked_pair (car lst)))
        (if (string=? elem (car checked_pair)) (cadr checked_pair) (match_head (cdr lst) elem) ) ) ))


(define (hashed_page arg table_size page_table page_size)
  (let ((page_parts (divide_address_space arg page_size)))
    (let ((hash_index (myhash (car page_parts) table_size) ))
      (let ((list_at_hash_index (get_element page_table hash_index)))
        (let ((found_page (match_head list_at_hash_index (car page_parts))))
          (if (null? found_page) '() (string-append found_page (cadr page_parts))) )))))


; 8- split_addresses
; (split_addresses "1110110101000000100100101011000101110011" 8)
(define (append_at_end element lst)
  (if (null? lst)       
      (list element)     
      (cons (car lst)    
            (append_at_end element (cdr lst)))))


(define (split_helper arr remaining size)
  (if (string=? remaining "") arr
      (split_helper (append_at_end (list (substring remaining 0 size)) arr) (substring remaining size) size) ))

(define (split_addresses args size) (split_helper '() args size))


; 9- map_addresses
; (map_addresses "001010000011001011000010100000011001011101001010" 5 '( ( ("1101" "010") ) ( ("0111" "111") ("0101" "000") ) ( ("1100" "101") ) ( ("1001" "100") ) ( ("0110" "110") ("0010" "001") ) ) 4 16)

(define (map_addresses args table_size page_table page_size address_space_size)
  (let ((split-args (split_addresses args address_space_size)))
    (map (lambda (arg) (hashed_page (car arg) table_size page_table page_size)) split-args)))

