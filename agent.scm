
; #********************************  CONTENT ***************************************# 

; PROGRAM     : AgentWorld

; DESCRIPTION : Implemented a software agent that will operate in a virtual environment, populated with 
;               vegetation, predators, and other agents. Agents can consume vegetation (when it is blooming) 
;               to increase their energy level. An agent’s energy level will also decrease over time based 
;               on their activity. 

; DEVELOPER   : Khetanshu chauhan

; #*******************************  STRATEGY OVERVIEW ********************************# 

; The idea is to first react based on an urgent situation, like if the agent is already attacked and still predator is next to the agent, 
; or if in-front there is a predator or agents see a bloomed veg in front of it, etc.
; If an urgent situation doesn't exist then the analyze and predict how safe is the precept that is received from the simulator.
; Based on the prediction, find the shortest path, using A* algorithm, that would cost least to reach the nearest bloomed vegetation.
; With the found path, next things is to generate sets of actions and save those for future turns.
; In future, these actions would be used directly if there isn't any urgent condition or showstopper.
; These actions would be reconsidered eat time, and if anytime program finds a cheaper set of actions then it would replenish 
; the action frontier (a queue) with the new sets of actions series


; #***********************************  API *****************************************# 

;ack simulator to begin the challenge
(define (initialize-agent) "OK")
      
;returns a logical actions valid for the current scenario to the simulator      
(define (choose-action current-energy previous-events percepts)
          (let* (
                 
                 (action (get-action current-energy previous-events percepts))
                 
                )
                action
          )
)

; #********************************  GLOBAL DATASTRUCTURE *****************************# 

;This QUEUE DATASTRUCTURE which store the list of future actions that needs to be passed to agent simulator;
;depending on various conditions; at any time if the actions found to be outdated data-struture would be flushed 
;and new actions would be loaded accordingly.
(define ACTION_FRONTIER '())
;This globally tracks the recent direction change that was made. So that in future if any direction change needed 
;then this information can be used
(define LAST_DIRECTION_ACTION '())
;This keeps the max energy that was given to this agent during the initialization
(define max-energy 0)

; #********************************  ACTION ANALYSIS & PREDICTION SERVICE ******************************# 

;This is the helper-function to choose-action API
;This would return a action that would best fit post considering the risks and profits
(define (get-action current-energy previous-events percepts)
  (let*
    (
      (isSafeToMoveForward 
                           (begin
                              (cond 
                                    ((equal? max-energy 0) (set! max-energy current-energy))
                              )      
                              (safeMoveForward?  percepts current-energy)
                           )
      )
      (vegInfo (findNearestNodeDistance percepts 'vegetation '()))
      ;vegatationInfo : (5 (3 2) (RIGHT) (vegetation 0 0)) == ((minDistToVeg) (relative co-ordinates) (relative direction) (node value))
      (minDistToVeg (car vegInfo))
      (veg_x (caadr vegInfo))
      (veg_y (cadadr vegInfo))
      (veg_relDirection (caaddr vegInfo))
      (minDistToPredator (car (findNearestNodeDistance percepts 'predator '())))
      (minDistToAgent (car (findNearestNodeDistance percepts 'agent '())))
      (agentNotPowerfull  (agentNotPowerfull? percepts current-energy))
      (srcNode '2)
      (destNode (convertRelativeCoordinateToListPosition (cadr vegInfo) (caddr vegInfo)))
    ) 
    ;Basic conditions
    (cond
      ;if attack in last chance and agent see a predator infornt then change to unique direction
      ( (and (is-attacked-in-last-chance previous-events) (is-front-predator? percepts)) (flush_ACTION_FRONTIER_and_send_command (get-new-direction)))
       ;if attack in last chance and agent dont see a predator infornt then move fast (MOVE-AGGRESSIVE-2)
      ( (and (is-attacked-in-last-chance previous-events)) (flush_ACTION_FRONTIER_and_send_command "MOVE-AGGRESSIVE-2"))
      ;if predator is at less than 2 blocks away then change to unique direction 
      ( (and (< minDistToPredator 3)) (flush_ACTION_FRONTIER_and_send_command (get-new-direction)))
      ;if agent see a barrier infornt then chance to unique direction
      ( (is-front-barrier? percepts) (flush_ACTION_FRONTIER_and_send_command (get-new-direction)))
      ;if agent don't see a movable place infornt i.e. if it dont see an empty or bloomed veg then turn to unique direction
      ( (and (not (is-front-empty? percepts))(not (is-front-vegetation-with-fruits? percepts))) 
        (flush_ACTION_FRONTIER_and_send_command (get-new-direction)) ) 
      ;if agent see a bloomed veg and no competition then EAT-PASSIVE
      ( (and (is-front-vegetation-with-fruits? percepts) (> minDistToAgent 2) (> minDistToPredator 2)) 
        (flush_ACTION_FRONTIER_and_send_command "EAT-PASSIVE"))
      ;if agent see a bloomed veg but have a competition when it can win then EAT-AGGRESSIVE
      ( (and (is-front-vegetation-with-fruits? percepts) (<= minDistToAgent 2) agentNotPowerfull (> minDistToPredator 2)) 
        (flush_ACTION_FRONTIER_and_send_command "EAT-AGGRESSIVE"))
      ;if agent see a bloomed veg but have a competition when it cannot win then turn to unique direction
      ( (and (is-front-vegetation-with-fruits? percepts) (<= minDistToAgent 2) (not agentNotPowerfull) (> minDistToPredator 2)) 
        (flush_ACTION_FRONTIER_and_send_command (get-new-direction)))
      ;if agent has less energy and percept is not dangerous and wasn't attacked in prev turn then STAY
      ( (and (not (is-attacked-in-last-chance previous-events)) (< current-energy (* .3 max-energy)) (not isSafeToMoveForward)(> minDistToPredator 4)) 
        (flush_ACTION_FRONTIER_and_send_command "STAY"))
      (#t 
         ;if the percepts safe to move ahead toward the vegetation with value (> than 0 or threshold)  
         (if isSafeToMoveForward
          (let* 
                (
                  (currentPerceptActions (generate-actions srcNode destNode percepts))
                  (isCurrentPerceptHasLessCost (if (> (length ACTION_FRONTIER) (length currentPerceptActions)) #t #f) )
                )
                (if (null? ACTION_FRONTIER)
                    ;*** Add all actions in ACTION_FRONTIER and send the first action ***
                    (begin
                      (addActions currentPerceptActions)
                      (refineActionForFightingSituation (removeAction) minDistToAgent)
                    )
                    ;remove the first action from ACTION_FRONTIER and return
                    (if isCurrentPerceptHasLessCost
                      (begin
                        (addActions currentPerceptActions)
                        (refineActionForFightingSituation (removeAction) minDistToAgent)
                      ) 
                      (refineActionForFightingSituation (removeAction) minDistToAgent)  
                    )
                )
          )
          ;** future expansion of the rules can be made here **
          (if (null? ACTION_FRONTIER)
            (cond 
              (#t (flush_ACTION_FRONTIER_and_send_command (get-new-direction)))
            )
            (begin
              ;as its a danger zone now  
              (set! ACTION_FRONTIER '())
              (flush_ACTION_FRONTIER_and_send_command (get-new-direction))
            )
          )    
        ) 
      )
    )  
  )
)


; #********************************  ACTION ANALYSIS & PREDICTION  SERVICE - HELPER-FUNCTIONS ******************************# 

;This would return if the agent was attacked in previous-events
(define (is-attacked-in-last-chance previous-events)
 (cond
   ((null? previous-events) #f)
   (#t (if (equal? (caar previous-events) 'attacked-by) #t (is-attacked-in-last-chance (cdr previous-events))))
 )
)  

;This would return if the agent close to bloomed vegetation is powerfull than itself 
(define (agentNotPowerfull? percepts current-energy)
  (if (list-contains? '(NOT_FOUND) (findNearestNodeDistance percepts 'agent '()) )
                             #t
                           (if (> (lg current-energy) 
                                  (caddr (cadddr (findNearestNodeDistance percepts 'agent '()))) 
                               )  
                               #t
                               #f
                           )
                         ) 
)

;This would return the absolute values between 1-35 of the precepts nodes. 
(define (convertRelativeCoordinateToListPosition coordinate direction)
    (let*
      (
        (x (car coordinate))
        (y (cadr coordinate))
        (direction (car direction))
      )
      (cond
        ( (equal? x 1)  (cond
                            ((equal? direction 'STRAIGHT) 2) 
                            ((equal? direction 'LEFT) (+ 0 y))
                            (#t (+ 2 y))
                        )
        )  
        ( (equal? x 2)  (cond
                            ((equal? direction 'STRAIGHT) 6)
                            ((equal? direction 'LEFT) (+ 3 y) )
                            (#t (+ 6 y))
                        )
        ) 
        ( (equal? x 3)  (cond
                            ((equal? direction 'STRAIGHT) 12)
                            ((equal? direction 'LEFT) (+ 8 y) )
                            (#t (+ 12 y))
                        )
        ) 
        ( (equal? x 4)  (cond
                            ((equal? direction 'STRAIGHT) 20)
                            ((equal? direction 'LEFT) (+ 15 y) )
                            (#t (+ 20 y))
                        )
        ) 
        ( (equal? x 5)  (cond
                            ((equal? direction 'STRAIGHT) 30)
                            ((equal? direction 'LEFT) (+ 24 y) )
                            (#t (+ 30 y))
                        )
        ) 
    )
  )
)

;Before sending the action to the agent simulator this function check if there exists a competition that the agent would win
;provided the action is EAT-PASSIVE. In this case this would convert it to EAT-AGGRESSIVE
(define (refineActionForFightingSituation action minDistToAgent)
  (if (and (equal? action 'EAT-PASSIVE) 
           (is-front-vegetation-with-fruits? percepts)
           (<= minDistToAgent 2)
      )
     'EAT-AGGRESSIVE
     (if (null? action) "STAY" action)
  )   
)  

;Return true if in-front of the agent there exists 'empty' space   
(define (is-front-empty? percepts)
  (if (equal? (cadar percepts) 'empty) #t #f)
)

;Returns true if in-front of agent there exists a 'barrier'
(define (is-front-barrier? percepts)
  (if (equal? (cadar percepts) 'barrier) #t #f)
)

;Returns true if in-front of agent there exists a 'vegetation-with-fruits'
(define (is-front-vegetation-with-fruits? percepts)
  (if (and  (list? (cadar percepts)) (> (length (cadar percepts)) 2) (equal? (caadar percepts) 'vegetation) (> (cadr (cdadar percepts)) 0)) #t #f)
)

;Returns true if in-front of agent there exists a 'agent'
(define (is-front-agent? percepts)
  (let 
      ((node (cadar percepts)))
      (if (and (list? node) (equal? (car node) 'agent))
          #t
          #f
      )
      
  )
)

;Returns true if in-front of agent there exists a 'predator'
(define (is-front-predator? percepts)
  (let 
      ((node (cadar percepts)))
      (if (and (list? node) (equal? (car node) 'predator))
          #t
          #f
      )
      
  )
)

; #********************************  SAFETY ANALYSIS SERVICE  ******************************# 

;This is an important service this program. This estimates if it would be safe to move-into the precepts 
;post considering various factors
(define (safeMoveForward?  percepts current-energy)
   (let*
     (
       ;find nearest valid vegetation
       (nearestVeg (findNearestNodeDistance percepts 'vegetation '()))
       (nearestVegPoint (cadr nearestVeg))
       (nearestVegDirection (caddr nearestVeg))
       (nearestVegDist  (car nearestVeg))
       
       ;find nearest predator
       (nearestPredator (findNearestNodeDistance percepts 'predator '()))
       (nearestPredatorPoint (cadr nearestPredator))
       (nearestPredatorDirection (caddr nearestPredator))
       (nearestPredatorDist  (car nearestPredator))
       
       (nearestPredatorToNearestVegDist (calDistanceBetwTypes nearestVegPoint nearestPredatorPoint nearestVegDirection  nearestPredatorDirection))
       
       
       ;find nearest Agent
       (nearestAgent (findNearestNodeDistance percepts 'agent '()))
       (nearestAgentPoint (cadr nearestAgent))
       (nearestAgentDirection (caddr nearestAgent))
       (nearestAgentDist  (car nearestAgent))
  
       
       (nearestAgentToNearestVegDist (calDistanceBetwTypes nearestVegPoint nearestAgentPoint nearestVegDirection  nearestAgentDirection))

       ;find out if the precept is safe to move-in       
       (isSafeToMoveForward (if (and (< nearestVegDist nearestPredatorToNearestVegDist) 
                                     (<= nearestVegDist nearestAgentToNearestVegDist)
                                     (agentNotPowerfull? percepts current-energy)
                                )
                                ;this case is 100% safe i.e. the vegation is closer than any risk
                                #t
                                ;otherwise this is likely unsafe and extra precautions actions need to be considered
                                #f
                            )
       )
     )
     (begin
        ; (display "nearestVegDist = ")(display nearestVegDist)(newline)
        ; (display "nearestPredatorToNearestVegDist = ")(display nearestPredatorToNearestVegDist)(newline)
        ; (display "nearestAgentToNearestVegDist = ")(display nearestAgentToNearestVegDist)(newline)
         isSafeToMoveForward
     )
   
     
   )
)

;Returns the distance between any two relative co-ordinates of points
(define (calDistanceBetwTypes pointA pointB directionA directionB)
  (if (equal? directionA directionB)
      (+ (abs (-  (car pointA) (car pointB)  ))   (abs (-  (cadr pointA) (cadr pointB)  )) )
      (+ (abs (-  (car pointA) (car pointB)  ))   (abs (+  (cadr pointA) (cadr pointB)  )) )
  )

)
                 
;Returns (manhattan distance to the type node, relative co-ordinate, and direction)
(define (findNearestNodeDistance percepts nodeType ignoreIds)           
  (let 
         (
           (tupple (findNearestNode percepts nodeType 1 ignoreIds))
         )
        (let 
            (
              (x (car tupple))
              (y (cadr tupple))
              (direction (caddr tupple))
              (node (cadddr tupple))
            ) 
            (list  (+ x y) (list x y ) (list direction) node)
        )
       
  )
  
)  

;Default x should be 1, ignoreIds = '()
;nodeType : vegetation, predator, etc.
;return relative x and y position of the vegetation in the percepts
(define (findNearestNode percepts nodeType x ignoreIds)
  (cond
    ((not (list? percepts)) #f) ;invalid input
    ((null? percepts ) (list 100 0 'NOT_FOUND (list nodeType)))
    
    (#t 
        (let*
          (
            (alist (car percepts))
            (limit (ceiling (/ (length alist) 2)))
            (result (findNode alist nodeType 1 limit -1 'NOTHING '() ignoreIds))
            (value (car result))
            (direction (cadr result))
            (node (caddr result))
            (y (if (< value limit)  
                   (- limit value)
                   (modulo value limit)
                )   
            )
          )  
          (if (>= value 0)
              (append (list x) (list y) (list direction) (list node))
              (findNearestNode (cdr percepts) nodeType (+ x 1) ignoreIds)
          )
        )
    )  
  )
)  

;Returns value%limit
(define (modulo value limit)
   (let*
      (
       (divi (floor (/ value limit)))
       (total (* divi limit))
      )
     (abs (- value total))
   ) 
)  


;Returns the relative location of the node(of any given type like vegation, predato or agent) within the percepts 
;Default pos = 1
(define (findNode alist type pos limit latestFoundPos direction latestFoundNode ignoreIds)
  (begin
    ; (display "pos = ")
    ; (display pos)
    ; (display " latestFoundPos =")
    ; (display latestFoundPos)
    ; (display " direction =")
    ; (display direction)
    ; (newline)
  (cond
    ((null? alist)  (if (>= latestFoundPos 0)
                      (list latestFoundPos direction latestFoundNode)
                      (list -1 direction '())
                  ))
    ;if the node found is in the ignored list then look forward   
    ( (and (list? (car alist))   (list-contains? (cadar alist) ignoreIds) )  
       (findNode (cdr alist) type (+ pos 1) limit latestFoundPos direction latestFoundNode ignoreIds)   
    )            
    ;special case if vegation has value 0 ;then simply look forward
    ( (and (list? (car alist)) (equal? (caar alist) 'vegetation) (equal? (caddar alist) 0) )  
       (findNode (cdr alist) type (+ pos 1) limit latestFoundPos direction latestFoundNode ignoreIds)   
    )            
    (#t
      (let*
        (
          (node (car alist))
          (currentPosdistanceToCenter (abs (- limit pos)))
          (latestFoundPosPosdistanceToCenter (abs (- limit latestFoundPos)))
          (leastDistancePos  (if (< currentPosdistanceToCenter latestFoundPosPosdistanceToCenter)  
                                 pos
                                 latestFoundPos
                              )
                            
          )
          (newdirection  (if (< currentPosdistanceToCenter latestFoundPosPosdistanceToCenter)  
                                   (cond 
                                     ((< pos limit) 'RIGHT)
                                     ((equal? pos limit) 'STRAIGHT)
                                     (#t 'LEFT)
                                    )  
                                  direction
                              )
            )
          (newFoundNode  (if (< currentPosdistanceToCenter latestFoundPosPosdistanceToCenter)  
                                 node
                                 latestFoundNode
                              )
                            
          )
          
        )
        (begin
          ; (display " node = ")
          ; (display node)
          ; (newline)
        
        (if (list? node) 
              (if (equal? (car node) type)
                  (findNode (cdr alist) type (+ pos 1) limit leastDistancePos newdirection newFoundNode ignoreIds)
                  (findNode (cdr alist) type (+ pos 1) limit latestFoundPos direction latestFoundNode ignoreIds)
              )
              (findNode (cdr alist) type (+ pos 1) limit latestFoundPos direction latestFoundNode ignoreIds)
            )
        )
      )  
    )
  ) 
)
)  


; #********************************  GRAPH - SEARCHING (A*) SERVICE  ******************************# 

(define tree '(
                (1      2  5)
                (2  1    3  6)
                (3  2      7)
                (4      5  10)
                (5  4  1  6  11)
                (6  5  2  7  12)
                (7  6  3  8  13)
                (8  7      14)
                (9      10  17)
                (10  9  4  11  18)
                (11  10  5  12  19)
                (12  11  6  13  20)
                (13  12  7  14  21)
                (14  13  8  15  22)
                (15  14      23)
                (16      17  26)
                (17  16  9  18  27)
                (18  17  10  19  28)
                (19  18  11  20  29)
                (20  19  12  21  30)
                (21  20  13  22  31)
                (22  21  14  23  32)
                (23  22  15  24  33)
                (24  23      34)
                (25      26  )
                (26  25  16  27  )
                (27  26  17  28  )
                (28  27  18  29  )
                (29  28  19  30  )
                (30  29  20  31  )
                (31  30  21  32  )
                (32  31  22  33  )
                (33  32  23  34  )
                (34  33  24  35  )
                (35  34      )
                
              )

)

(define destToSrcCostSorted '(
                ;left         
                (1  (2 1) (5 4))
                ;mid
                (2  (1 3) (3 3)  (6 4))
                ;right
                (3  (2 1) (7 4))
                ; left
                (4  (5 1) (10 4))
                (5  (6 1) (1 2) (4 4)  (11 4))
                ; mid
                (6   (2 1) (5 3)  (7 3)  (12 4))
                ;right
                (7  (6 1) (3 2)  (8 4)  (13 4))
                (8  (7 1) (14 4))
                ;left
                (9   (10 1)  (17 4))
                (10  (11 1) (4 2) (9 4) (18 4))
                (11   (12 1) (5 2) (10 4)   (19 4))
                ;mid
                (12   (6 1) (11 3)  (13 3)  (20 4))
                ;right
                (13  (12 1)  (7 2) (14 4)  (21 4))
                (14  (13 1)  (8 2)  (15 4)  (22 4))
                (15  (14 1)    (23 4))
                ;left
                (16  (17 1)  (26 4))
                (17  (18 1) (9 2)    (16 4)   (27 4))
                (18  (19 1) (10 2)   (17 4)    (28 4))
                (19  (20 1) (11 2)   (18 4)   (29 4))
                ;mid
                (20  (12 1) (19 3) (21 3)  (30 4))
                ;right
                (21  (20 1)  (13 2)  (22 4)  (31 4))
                (22  (21 1)  (14 2)  (23 4)  (32 4))
                (23  (22 1)  (15 2)  (24 4)  (33 4))
                (24  (23 1) (34 4))
                ;left
                (25  (26 1))
                (26  (27 1) (16 2)   (25 4) )
                (27  (28 1) (17 2)  (26 4) )
                (28  (29 1) (18 2)   (27 4) )
                (29  (30 1) (19 2)   (28 4) )
                ;mid
                (30  (20 1) (29 3)    (31 3))
                ;right
                (31  (30 1)  (21 2)  (32 4))
                (32  (31 1)  (22 2)  (33 4))
                (33  (32 1)  (23 2)  (34 4))
                (34  (33 1)  (24 2)  (35 4))
                (35  (34 1))
              )

)

;Generates actions list based on the path found the the neareast bloomed vegetation in the given percepts
(define (generate-actions srcNode destNode percepts)
  (let*
    (
      (path (searchPathToValidVeg srcNode destNode tree percepts))
      (actions (if (null? path) path (convertPathToActions (car path) 'F path)))
    )
    (begin
      ; (display "path = ")(display path)(newline)
      actions
    )
  )
)

;Converts the path found to the neareast bloomed vegetation to set of actions like MOVE-PASSIVE-1, TURN-LEFT, etc. 
;usage : (convertPathToActions (car path) 'F (cdr path))
;@prevDirection : R, L, F, B (i.e. RIGHT, LEFT, FORWARD, BACKWARD)
(define (convertPathToActions parent prevDirection path)
  (begin
    ; (display "parent = ")(display parent)(newline)
    ; (display "prevDirection = ")(display prevDirection)(newline)
    ; (display "path = ")(display path)(newline)
    ; (newline)
  
    (cond
      ((null? path) path)
      (#t 
          (let*
              (
                (child (car path))
                (difference (- parent child))
                (inSameLevel (if (equal? (abs difference) 1) #t #f))
              )
              (let*
                (
                  (directionChange (cond
                                       ;forward direction combinations
                                       ( (and  (equal?  prevDirection 'F) inSameLevel)
                                         (if (positive? difference) '("TURN-LEFT" L) '("TURN-RIGHT" R) ))
                                       
                                       ;left direction combinations
                                        ( (and  (equal?  prevDirection 'L) (not inSameLevel))
                                         (if (positive? difference)  '("TURN-LEFT" B) '("TURN-RIGHT" F) ))  
                                       
                                       ;right direction combinations
                                       ( (and  (equal?  prevDirection 'R) (not inSameLevel))
                                         (if (positive? difference)  '("TURN-RIGHT" B) '("TURN-LEFT" F)  ))  
                                       ;backward direction combinations
                                       ( (and  (equal?  prevDirection 'B) inSameLevel)
                                         (if (positive? difference)  '("TURN-RIGHT" L) '("TURN-LEFT" R)  ))  
                                         
                                         
                                       ;same direction (move-forward scenario) combinations 
                                       (#t '(FORWARD)) 
                                  )
                  )
                  (moveAction  (if (equal? (car directionChange)  'FORWARD) '() (list (car directionChange))))
                  (newDirectionChange (if (equal? (car directionChange) 'FORWARD) prevDirection (cadr directionChange)) )
                  ; (newDirectionChange (cadr directionChange))            
                )
                (begin
                  ; (display "directionChange = ")(display directionChange)(newline)
                  ; (newline)
                
                  (append
                      moveAction
                      ;add actions
                      (if (null? (cdr path)) '("EAT-PASSIVE") '("MOVE-PASSIVE-1"))
                      (convertPathToActions child newDirectionChange (cdr path))
                  )
                )
              
              )
          ) 
      )
    )
  )
)

;Searches a path to the  neareast bloomed vegetation
(define (searchPathToValidVeg srcNode destNode tree percepts)
  (let* 
      (
        (mapping (generateMap tree))
        (path 
              
              (begin
                ; (display "mapping  = ")(display mapping)(newline)(newline)
                ;if path not possible then NULL would be received
                 (find-path destNode srcNode  mapping percepts '() '())
               )
        )
      )
     (begin
      ; (display "mapping  = ")(display mapping)(newline)
      (reverse path)
     )
  )
)

;Generates a Map of the defined tree
(define (generateMap tree)
  (cond
    ((null? tree) '())
    (#t (let*
          (
            (node (car tree))
            (childern (cdr node))
            (parent (car node))
            (childernParentMap (generateChildernParentMap parent childern))
          )      
          (append childernParentMap (generateMap (cdr tree)))
        )
    )
  )
)

;Generates childern of the parent
(define (generateChildernParentMap parent childern)
  (cond
    ((null? childern) '())
    (#t (append (list (list (car childern) parent)) (generateChildernParentMap parent (cdr childern))))
  )  
)


(define (find-path srcNode destNode  mapping percepts foundPath invalidNodesList)
  (begin
    ; (newline)(display "foundPath = ")(display foundPath)(newline)
    ; (display "srcNode = ")(display srcNode)(newline)
    (cond
      ((null? mapping) '())
      (#t
        (let*
            (
              (node (A*searchNode mapping percepts srcNode '() foundPath invalidNodesList) )
              (child (if (null? node) node (car node)))
              (parent (if (null? node) node (cadr node)))
            )
           (begin
            ; (display "# node = ")(display node)(newline)
            ; (display "mapping = ")(display mapping)(newline)
             (if (null? node)
                (if (null? foundPath)
                    '()
                    ;i.e. the last src 
                    (let*
                         (
                            (newInvalidNodesList (append invalidNodesList (list srcNode)))
                            (reversedFoundPath  (reverse foundPath))
                            (newParent (car reversedFoundPath))
                            (updatedFoundPath  (reverse (cdr reversedFoundPath)))
                         )
                         (begin
                         
                          ; (display "newParent = ")(display newParent)(newline)
                          ; (display "destNode = ")(display destNode)(newline)
                          ; (display "updatedFoundPath = ")(display updatedFoundPath)(newline)
                          ; (display "newInvalidNodesList = ")(display newInvalidNodesList)(newline)
                          
                          (find-path newParent destNode mapping percepts updatedFoundPath newInvalidNodesList)
                         )
                    )
                )
                ; node
              (if (equal? parent destNode)
                (append   foundPath node)
                (find-path parent destNode  mapping percepts (append foundPath (list child)) invalidNodesList)
              )
             )
           )
        )
      )
    )
  )
)

;Searches expectedNode in the given context using the A* algorithm considering total number of actions as a heuristic function 
(define (A*searchNode mapping percepts expectedNode leastParentValue foundPath invalidNodesList)
  (begin
    ; (display "$ FrontNode = ") (display (if (null? mapping) '() (caar mapping) )) (newline)
    ; (display "$ invalidNodesList = ")(display invalidNodesList)(newline)
    ; (display " $ leastParentValue = ")(display leastParentValue)(newline)
    (cond
      ((null? mapping) leastParentValue)
      (#t 
          (if (and (equal? (caar mapping) expectedNode) 
                   (not (list-contains? (cadar mapping) foundPath))
                   (isNodeSafeAt percepts (cadar mapping))
                   (not (list-contains? (cadar mapping) invalidNodesList))             
                                 
              )
              (let*
                (
                  (oldMinCost (if (null? leastParentValue) 
                                  100
                                 (getEdgeCost destToSrcCostSorted (car leastParentValue) (cadr leastParentValue)) 
                              )
                  )
                  (newMinCost (getEdgeCost destToSrcCostSorted (caar mapping) (cadar mapping)))
                )
                (if (< newMinCost oldMinCost)
                  ; (car mapping)  
                  (A*searchNode (cdr mapping) percepts expectedNode (car mapping) foundPath invalidNodesList)
                  (A*searchNode (cdr mapping) percepts expectedNode leastParentValue foundPath invalidNodesList)
                )  
              )
              (A*searchNode (cdr mapping) percepts expectedNode leastParentValue foundPath invalidNodesList)
          )
          
      )
    )
  )
)

;Gives the cost to reach a adjacent node of a source node
(define (getEdgeCost destToSrcCostSorted src dest)
  (let*
    (
     (edges (searchCostNode destToSrcCostSorted src)) 
     (cost (searchCostNode edges dest))
    )
   (car cost)
  )
)  

;finds the cost of a node
(define (searchCostNode destToSrcCostSorted expectedNode )
  (cond
    ((null? destToSrcCostSorted) '())
    (#t (if  (equal? (caar destToSrcCostSorted) expectedNode)
          (cdar destToSrcCostSorted)  
          (searchCostNode (cdr destToSrcCostSorted) expectedNode)
        )    
        
    )
  )
)

;Returns the childern of a parent node
(define (getChildern parentNode tree)
        (cond
            ((null? tree) tree)
            ((equal? (car(car tree)) parentNode) (cdr (car tree)))
            (#t (getChildern parentNode (cdr tree)))
        )
) 

; #********************************  GRAPH - SEARCHING (A*) HELPER-FUNCTIONS  ******************************# 

;Return if a node at given position is safe i.e. not agent, or predator etc.
(define (isNodeSafeAt percepts i)
  (let*
    (
      (node (getPercentAt percepts i))
      (type (if (list? node) (car node) '()))
    )
    (cond 
          ((equal? type 'predator) #f)
          ((equal? type 'agent) #f)
          ((equal? type 'barrier) #f)
          ((equal? type 'vegetation) #f)
          (#t #t)
    )
  
  )
)

;Returns the node at i in the given precept
(define (getPercentAt percepts i)
  (cond
    ((null? percepts) percepts)
    (#t 
        (let*
          (
            (alist (car percepts))
            (size  (length alist))
            (remainingCnt  (- i size))
          )
          (if (<= remainingCnt 0)
            (nth-item i alist)  
            (getPercentAt (cdr percepts) remainingCnt)
          )
        )
    )
  )
)  

; #********************************  GLOBAL DATASTRUCTURE HELPER-FUNCTIONS ******************************# 

;Returns the front action from the ACTION_FRONTIER - but dont remove it
(define (seekAction)
  (cond
    ((null? ACTION_FRONTIER) ACTION_FRONTIER)
    (#t 
        (let
        (
          (action (car ACTION_FRONTIER))
        )
        action
      )
    )
  )
) 

;Returns the front action from the ACTION_FRONTIER and remove it
(define (removeAction)
  (cond
    ((null? ACTION_FRONTIER) ACTION_FRONTIER)
    (#t
      (let
        (
          (action (car ACTION_FRONTIER))
        )
        (begin
          (set! ACTION_FRONTIER (cdr ACTION_FRONTIER))
          action
        )
      )
    )
  )
)  

;adds an action to ACTION_FRONTIER
(define (addActions actions)
  (set! ACTION_FRONTIER (append ACTION_FRONTIER actions))
)

;returns a new unique that has not been taken recently in 2 turns and updates LAST_DIRECTION_ACTION datastruture
(define (get-new-direction)
  (let*
    ((newDirectionAction (find-new-direction)))
    (begin
      (set! LAST_DIRECTION_ACTION newDirectionAction)
      newDirectionAction
    )
  )
)

;find a unique direction that has not been taken recently in 2 turns
(define (find-new-direction)
  (cond
    ( (equal? (negate-direction LAST_DIRECTION_ACTION) "TURN-LEFT") "TURN-LEFT")
    ( (equal? (negate-direction LAST_DIRECTION_ACTION) "TURN-RIGHT") "TURN-AROUND")
    ( (equal? (negate-direction LAST_DIRECTION_ACTION) "TURN-AROUND") "TURN-RIGHT")
    (#t "TURN-AROUND")
  )
)


;returns the negated value of the direction made, like TURN-LEFT -> TURN-RIGHT
(define (negate-direction directionAction)
  (cond
    ((equal? directionAction "TURN-LEFT") "TURN-RIGHT")
    ((equal? directionAction "TURN-RIGHT") "TURN-LEFT")
    ((equal? directionAction "TURN-AROUND") "TURN-AROUND")
  )  
)

;updates the LAST_DIRECTION_ACTION with new value
(define (update_LAST_DIRECTION_ACTION newDirectionAction)
  (set! LAST_DIRECTION_ACTION newDirectionAction)
)

;resets the ACTION_FRONTIER and then sends the action/command 
(define (flush_ACTION_FRONTIER_and_send_command newCommand)
 (begin
   (set! ACTION_FRONTIER '())
  newCommand
 )
)

; #********************************  GENERIC LIBRARIES ******************************# 

;#INPUT
;@value : value that needs to be checked
;@alist : a list
;#RETURNS
; #t of #f depending if value is present in alist
(define (list-contains? value alist)
        (cond
                ((null? alist) #f)
                (#t (if (equal? (car alist) value) #t (list-contains? value (cdr alist))))
        )
)


;#INPUT 
;alist = list 1 example (1 2 3)
;blist = list 2 example (x 2 y)
;#RETURNS
; list contain intersection of two list example here (2)
(define (find-intersection-list alist blist)
        (cond
                ((null? alist) alist)
                (#t (append (if (list-contains? (car alist) blist) (list (car alist)) '())
                                (find-intersection-list (cdr alist) blist))
                )
        )
)


;#INPUT
;@i = first position
;@j = second position
;@alist = a list
;#RETURNS
;swapped value list
(define (swap-elements i j alist)
  (cond
    ((null? alist) alist)
    ((or (< i 1) (> j (length alist)))alist)
    ((= i j) alist)
    (#t 
        (let (
               (blist-i (list (nth-item i alist)))
               (blist-j (list (nth-item j alist)))
             )
             (begin
                 (replace-nth-item (replace-nth-item alist blist-i j) blist-j i)
             )
        )
    )
  )
)

;#INPUT
;@n = position
;@alist = a list
;#RETURNS
;nth-item
(define (nth-item n alist)
        (cond
                ((null? alist) alist)
                (#t (if (= n 1) (car alist)
                        (nth-item (- n 1) (cdr alist))))
))


;#INPUT
;@n = position
;@alist = a list
;@alist = a list that need to need added in placed of alist 
;#RETURNS
;replaced nth item list
(define (replace-nth-item alist blist n)
        (cond
                ((null? alist) alist)
                ((= n 1) (append blist (cdr alist)))
                (#t (append (list (car alist)) (replace-nth-item  (cdr alist) blist (- n 1))))
))

;Returns the log2(x)
(define (lg number)
  (/ (log number) (log 2))  
)





