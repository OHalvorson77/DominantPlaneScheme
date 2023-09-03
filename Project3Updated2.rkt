#lang scheme
;Owen Halvorson
;0300251644

;Function to read an input file
;Input: Filename
;Output: A list of points given by file
(define (readXYZ fileIn) 
 (let ((sL (map (lambda s (string-split (car s))) 
 (cdr (file->lines fileIn)))))
 (map (lambda (L)
 (map (lambda (s)
 (if (eqv? (string->number s) #f)
 s
(string->number s))) L)) sL)))



;Function to find a plane equation from three points
;Input: Three points
;Output: Plane equation values (A, B, C, D)

(define (plane P1 P2 P3)
  ;Initialze v1 as Point1 - Point 2 and V2 as point1- point3
  (let ((v1 (list (- (car P2) (car P1))
                  (- (cadr P2) (cadr P1))
                  (- (caddr P2) (caddr P1))))
        (v2 (list (- (car P3) (car P1))
                  (- (cadr P3) (cadr P1))
                  (- (caddr P3) (caddr P1)))))
    (let* ((planeEquation (crossProduct v1 v2));Initialize planeEquation as the crossproduct of v1 and v2 
          (d (- 0(* (car P1) (car planeEquation));Isolate for d using a,b and c values
                (*(cadr P1) (cadr planeEquation))
                (*(caddr P1) (caddr planeEquation)))))
      (list (car planeEquation) (cadr planeEquation) (caddr planeEquation) d))));return a list of the a,b,c and d values of the plane equation

;Function used to calculate the crossproduct of two vectors
;Input: Two vectors (V1 and V2)
;Output: The crossproduct of the two vectors as a list of 3 values
(define (crossProduct v1 v2)
  ;Make a list of the a b and c values after the crossproduct
  (list (- (* (cadr v1) (caddr v2)) (* (cadr v2) (caddr v1)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))))




;Function to calculate number of iterations needed to find dominant plane

;Input: Confidence value, percentage value
;Output: Number of iterations it takes to find dominant plane
(define (ransacNumberOfIteration confidence percentage)
  (let ((k (log (- 1 confidence))))
    (exact-round (/ k (log (- 1 (expt percentage 3)))))))





;Method to get the support of a plane
;Plane values(A,B,C,D), list of points, maximum distance from point to plane(eps), count value
;Output; The plane values with the number of points it supports at front of list
(define (support plane points eps count)
  (if
   (not (null? (cdr points)));If the elements after the first element isnt null
   (if
    (> eps (abs(/(+(*(car plane)(car (car points)))(*(cadr plane)(cadr (car points)))(*(caddr plane)(caddr (car points)))(cadddr plane))(sqrt(+(expt(car plane)2)(expt(cadr plane)2)(expt(caddr plane)2)
      )))));If the front point in the list is less then eps distance from the plane run the support function again with the first element removed and count +1
    (support plane (cdr points) eps (+ 1 count))
    (support plane (cdr points) eps count);If point isn't within eps from plane recursively run function again without first element
    )
   (cons count plane);If list is null construct a list of the count as the car and the plane as the cdr
   )
  )

;Method to determine dominant plane
;Input:List of points, number of iterations(k), maximum distance from plane to point(eps), best supportplane value
;Output: Best plane support and a b c d
(define (dominantPlane Ps k eps best)
  (if
   (positive? k);if k is positive do the following
   (let*((currentPlane(plane(list-ref Ps (random (length Ps)))(list-ref Ps (random (length Ps)))(list-ref Ps (random (length Ps)))));Find the current plane with three randomly selected points
         (supportOfPlane(support currentPlane Ps eps 0));Run the support of plane method to find the support of the current plane
         )
     (if
      (>(car supportOfPlane) (car best));supportOfPlane support value is larger then the best plane support value recursively run dominantPlane method with one less k and supportOfPlane as best plane
      (dominantPlane Ps (- 1 k) eps supportOfPlane)
      (dominantPlane Ps (- 1 k) eps best);Else call dominant plane with same  best plane and one less k
      )
     )
   best;if k is negative return the best plane
   )
  )


;Method to run the program
;Input:Name of file, confidence value, percentage of points on plane value
;Output:The most dominant plane values, given by dominantPlane Function
(define (planeRANSAC filename confidence percentage eps)
  (let ((ps(readXYZ filename));Initializes the points with the readXYZ function
        [k(ransacNumberOfIteration confidence percentage)];Calculates the number of iterations needed with the given confidence and percentage of points on plane
        )
    
    (dominantPlane ps k eps '(0 '(0 0 0 0)));Runs the dominant plane method with the points, the iterations, the eps and a base dominant plane of 0 values
    )
  )



      



                      
                   



  