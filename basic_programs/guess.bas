100 LET R = RND(10)
110 INPUT "GUESS A NUMBER BETWEEN 1 AND 10"; G
120 IF G = R THEN GOTO 180
130 IF G < R THEN GOTO 160
140 INPUT "TOO HIGH.  GUESS AGAIN"; G
150 GOTO 120
160 INPUT "TOO LOW. GUESS AGAIN"; G
170 GOTO 120
180 PRINT "YOU WIN!"
190 END
