#lang racket
(take '(a b c d) 3) ; take left elements
(takef '(2 4 5 8) even?)
(list-ref '(a b c d) 3) ; get nth elements
(take-right '(1 2 3 4) 2);  take right elements
(drop-right '(1 2 3 4) 2) ; drop the right,return left
(map + '(1 2 3 4) '(1 2 3 4))