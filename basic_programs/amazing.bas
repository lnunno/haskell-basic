(define amazing
'((10 PRINT TAB(28) "AMAZING PROGRAM")
  (20 PRINT TAB(15) "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY")
  (60 PRINT)
  (100 INPUT "WHAT IS YOUR WIDTH" H)
  (105 INPUT "WHAT IS YOUR HEIGHT" U)
  (110 DIM W(H U))
  (115 DIM V(H U))
  (150 PRINT)
  (160 LET Q = 0)
  (161 LET Z = 0)
  (162 LET X = INT(((RND(1) * H) + 1)))
  (165 FOR I1 = 1 TO H)
  (170 IF (I1 = X) THEN 173)
  (171 PRINT! ".--")
  (172 GOTO 180)
  (173 PRINT! ".  ")
  (180 NEXT I1)
  (190 PRINT ".")
  (195 LET C = 1)
  (196 LET W(X 1) = C)
  (197 LET C = (C + 1))
  (200 LET R = X)
  (202 LET S = 1)
  (205 GOTO 260)
  (210 IF (R <> H) THEN 240)
  (215 IF (S <> U) THEN 230)
  (220 LET R = 1)
  (222 LET S = 1)
  (225 GOTO 250)
  (230 LET R = 1)
  (232 LET S = (S + 1))
  (235 GOTO 250)
  (240 LET R = (R + 1))
  (250 IF (W(R S) = 0) THEN 210)
  (260 IF ((R - 1) = 0) THEN 530)
  (265 IF (W((R - 1) S) <> 0) THEN 530)
  (270 IF ((S - 1) = 0) THEN 390)
  (280 IF (W(R (S - 1)) <> 0) THEN 390)
  (290 IF (R = H) THEN 330)
  (300 IF (W((R + 1) S) <> 0) THEN 330)
  (310 LET X = INT(((RND(1) * 3) + 1)))
  (320 ON X GOTO 790 820 860)
  (330 IF (S <> U) THEN 340)
  (334 IF (Z = 1) THEN 370)
  (338 LET Q = 1)
  (339 GOTO 350)
  (340 IF (W(R (S + 1)) <> 0) THEN 370)
  (350 LET X = INT(((RND(1) * 3) + 1)))
  (352 ON X GOTO 790 820 910)
  (370 LET X = INT(((RND(1) * 2) + 1)))
  (380 ON X GOTO 790 820)
  (390 IF (R = H) THEN 470)
  (400 IF (W((R + 1) S) <> 0) THEN 470)
  (405 IF (S <> U) THEN 420)
  (410 IF (Z = 1) THEN 450)
  (415 LET Q = 1)
  (416 GOTO 430)
  (420 IF (W(R (S + 1)) <> 0) THEN 450)
  (430 LET X = INT(((RND(1) * 3) + 1)))
  (435 ON X GOTO 790 860 910)
  (450 LET X = INT(((RND(1) * 2) + 1)))
  (455 ON X GOTO 790 860)
  (470 IF (S <> U) THEN 490)
  (480 IF (Z = 1) THEN 520)
  (485 LET Q = 1)
  (486 GOTO 500)
  (490 IF (W(R (S + 1)) <> 0) THEN 520)
  (500 LET X = INT(((RND(1) * 2) + 1)))
  (510 ON X GOTO 790 910)
  (520 GOTO 790)
  (530 IF ((S - 1) = 0) THEN 670)
  (540 IF (W(R (S - 1)) <> 0) THEN 670)
  (545 IF (R = H) THEN 610)
  (547 IF (W((R + 1) S) <> 0) THEN 610)
  (550 IF (S <> U) THEN 560)
  (552 IF (Z = 1) THEN 590)
  (554 LET Q = 1)
  (555 GOTO 570)
  (560 IF (W(R (S + 1)) <> 0) THEN 590)
  (570 LET X = INT(((RND(1) * 3) + 1)))
  (575 ON X GOTO 820 860 910)
  (590 LET X = INT(((RND(1) * 2) + 1)))
  (600 ON X GOTO 820 860)
  (610 IF (S <> U) THEN 630)
  (620 IF (Z = 1) THEN 660)
  (625 LET Q = 1)
  (626 GOTO 640)
  (630 IF (W(R (S + 1)) <> 0) THEN 660)
  (640 LET X = INT(((RND(1) * 2) + 1)))
  (650 ON X GOTO 820 910)
  (660 GOTO 820)
  (670 IF (R = H) THEN 740)
  (680 IF (W((R + 1) S) <> 0) THEN 740)
  (685 IF (S <> U) THEN 700)
  (690 IF (Z = 1) THEN 730)
  (695 LET Q = 1)
  (696 GOTO 830)
  (700 IF (W(R (S + 1)) <> 0) THEN 730)
  (710 LET X = INT(((RND(1) * 2) + 1)))
  (720 ON X GOTO 860 910)
  (730 GOTO 860)
  (740 IF (S <> U) THEN 760)
  (750 IF (Z = 1) THEN 210)
  (755 LET Q = 1)
  (756 GOTO 770)
  (760 IF (W(R (S + 1)) <> 0) THEN 210)
  (770 GOTO 910)
  (790 LET W((R - 1) S) = C)
  (800 LET C = (C + 1))
  (801 LET V((R - 1) S) = 2)
  (802 LET R = (R - 1))
  (810 IF (C = ((H * U) + 1)) THEN 1010)
  (815 LET Q = 0)
  (816 GOTO 260)
  (820 LET W(R (S - 1)) = C)
  (830 LET C = (C + 1))
  (840 LET V(R (S - 1)) = 1)
  (841 LET S = (S - 1))
  (842 IF (C = ((H * U) + 1)) THEN 1010)
  (850 LET Q = 0)
  (855 GOTO 260)
  (860 LET W((R + 1) S) = C)
  (870 LET C = (C + 1))
  (872 IF (V(R S) = 0) THEN 880)
  (875 LET V(R S) = 3)
  (876 GOTO 890)
  (880 LET V(R S) = 2)
  (890 LET R = (R + 1))
  (900 IF (C = ((H * U) + 1)) THEN 1010)
  (905 GOTO 530)
  (910 IF (Q = 1) THEN 960)
  (920 LET W(R (S + 1)) = C)
  (921 LET C = (C + 1))
  (922 IF (V(R S) = 0) THEN 940)
  (930 LET V(R S) = 3)
  (932 GOTO 950)
  (940 LET V(R S) = 1)
  (950 LET S = (S + 1))
  (952 IF (C = ((H * U) + 1)) THEN 1010)
  (955 GOTO 260)
  (960 LET Z = 1)
  (970 IF (V(R S) = 0) THEN 980)
  (975 LET V(R S) = 3)
  (976 LET Q = 0)
  (977 GOTO 210)
  (980 LET V(R S) = 1)
  (982 LET Q = 0)
  (983 LET R = 1)
  (984 LET S = 1)
  (985 GOTO 250)
  (1010 FOR J1 = 1 TO U)
  (1011 PRINT! "I")
  (1012 FOR I2 = 1 TO H)
  (1015 IF (V(I2 J1) < 2) THEN 1030)
  (1020 PRINT! "   ")
  (1021 GOTO 1040)
  (1030 PRINT! "  I")
  (1040 NEXT I2)
  (1041 PRINT)
  (1043 FOR I3 = 1 TO H)
  (1045 IF (V(I3 J1) = 0) THEN 1060)
  (1050 IF (V(I3 J1) = 2) THEN 1060)
  (1051 PRINT! ":  ")
  (1052 GOTO 1070)
  (1060 PRINT! ":--")
  (1070 NEXT I3)
  (1071 PRINT ".")
  (1072 NEXT J1)
  (1073 END)))
