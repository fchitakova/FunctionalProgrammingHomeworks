This repo contains some of homeworks tasks given in 2019-2020 Functional Programming Course in FMI,Sofia University

Conditions :

1. Fun With Digits
 #lang racket

; Искаме да дефинираме следните имена: one, two, three, ..., nine, plus, minus, times, div,
; така че извиквания от типа на (one (plus (three))) (операция с точно две операнди) да връщат легитимни числови стойности (в този случай - 4)
; Още малко примери:
; (three (times (five))) -> 15
; (nine (div (three))) -> 3
; (eight (minus (four))) -> 4

2.Prefixes 
; Искаме да намерим всички префикси на даден списък
; Например за '(1 2 3), това са '(), '(1), '(1 2) и '(1 2 3)

(define (prefixes xs)
  (void))
