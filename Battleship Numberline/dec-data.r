
dec.data <- read.csv("dec-data.csv",header=T)

str(dec.data)

'data.frame':   8257 obs. of  32 variables:

 $ X                    : int  1 2 3 4 5 6 7 8 9 10 ...
                          unique row number for every row in the data set

 $ SID                  : int  161461 161461 ...
                          unique identifier for each player; see also sID

 $ ip                   : Factor w/ 280 levels "108.66.1.69 ",...
                          unique identifier for each computer used;
                          see also ip2 and ip3

 $ userName             : Factor w/ 414 levels "User 161461",...
                          same as SID

 $ experimentName       : Factor w/ 15 levels "Decimal_10thsG",...
                          name of experiment for this participant x question
                          interaction

 $ levelName            : Factor w/ 7 levels "Decimal_10thsG",...
                          groups of experiment names

 $ currentLevelNo       : int  2 2 2 2 3 3 3 3 3 3 ...
                          game level reported to player

 $ totalTrials          : int  20 20 20 20 20 20 20 20 20 20 ...
                          maximum number of questions allowed per student

 $ isGuidesEnabled      : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
                          are hashmarks printed on the numberline
                          that players see?

 $ currentQuestion      : num  0.83 0.63 0.43 0.5 ...
                          correct location of the submarine as a number on
                          the number line. These uniquely identify the
                          questions that students see.

 $ answerByUser         : Factor w/ 7372 levels "(-1#-1)","(0 # 0)",...
                          internal coding for location of player's mouse
                          click on screen

 $ answerDec            : num  0.849 0.591 0.402 0.497 0.58 ...
                          location of player's mouse click as a number on
                          the number line

 $ hitType              : Factor w/ 5 levels "Miss!!","Near Miss!!",...
                          "Perfect Hit!!" = answerDec is close enough to
                          currentQuestion to count as a "correct answer";
                          all other responses are "wrong" or "partial credit"

 $ currentTrialStartTime: Factor w/ 7145 levels "Fri Nov 11 00:00:04 2011 UTC"...
                          timestamp at start of current question

 $ currentTrialEndTime  : Factor w/ 6842 levels "-1","Fri Nov 11 00:00:09 2011 UTC"...
                          timestamp at end of current question

 $ currentAccuracy      : num  98.1 96.1 97.2 99.7 98 ...
                          a measure of accuracy of player's response

 $ avgAccuracy          : num  97.4 97.3 97.3 97.4 98 ...
                          running average of "currentAccuracy" over the
                          questions each player attempted

 $ curReactionTime      : Factor w/ 4046 levels "0.016 secs","0.024 secs",...
                          The time it took for player to produce an answer
                          on this question; see also "reacTime" below.

 $ totalTime            : Factor w/ 8147 levels "10.324 Secs",...
                          Not sure; this may be the time from the start
                          of one question to the start of the next question...

 $ timeLimit            : int  10 10 10 10 10 10 10 10 10 10 ...
                          maximum number of second allowed, per question.
resp                          if curReactionTime > timeLimit, then hitType =
                          "Time Out!!", although this rule does not seem
                          to be strictly enforced.

 $ avgTime              : num  2.49 2.47 2.47 2.44 3.54 ...
                          Running average of curReactionTime through each
                          player's session with the game

 $ bestTime             : num  1.39 1.39 1.39 1.39 3.54 ...
                          Minimum of curReactionTime through each
                          player's session with the game

 $ currentStarCount     : int  13 13 14 15 1 2 2 3 4 5 ...
                          Cumulative number correct ("Perfect Hit!!") in the
                          current session

 $ totalStarCount       : int  688 701 715 730 746 748 750 753 757 762 ...
                          seems to be a cumulative sum of currentStarCount...

 $ fireType             : Factor w/ 2 levels "","CLICK": 2 2 2  ...
                          Did player respond with a mouse click or a key press?

 $ sound                : Factor w/ 2 levels "OFF","ON": 2 2 2 2 2 2 2 2 2 2 ...
                          Is the sound on or off in the game?

 $ resp                 : int  1 0 1 1 1 1 0 1 1 1 ...
                          resp = 1 if hitType="Perfect Hit!!", else resp = 0.

 $ reacTime             : num  2.05 2.23 2.32 1.96 3.54 ...
                          numerical value of currentReactionTime

 $ ip2                  : num  12.2 12.2 12.2 12.2 12.2 ...
                          first two numbers in ip address

 $ ip3                  : Factor w/ 256 levels "108.66.1", ...
                          first three numbers in ip address
