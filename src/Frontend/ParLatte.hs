{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Frontend.ParLatte where
import Frontend.AbsLatte as AbsLatte
import Frontend.LexLatte
import Frontend.ErrM
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (String)
	| HappyAbsSyn7 (Program)
	| HappyAbsSyn8 (TopDef)
	| HappyAbsSyn9 ([TopDef])
	| HappyAbsSyn10 (Arg)
	| HappyAbsSyn11 ([Arg])
	| HappyAbsSyn12 (Block)
	| HappyAbsSyn13 ([Stmt])
	| HappyAbsSyn14 (Stmt)
	| HappyAbsSyn15 (Item)
	| HappyAbsSyn16 ([Item])
	| HappyAbsSyn17 (Type)
	| HappyAbsSyn18 ([Type])
	| HappyAbsSyn19 (Expr)
	| HappyAbsSyn26 ([Expr])
	| HappyAbsSyn27 (AddOp)
	| HappyAbsSyn28 (MulOp)
	| HappyAbsSyn29 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (50) = happyShift action_7
action_0 (54) = happyShift action_8
action_0 (56) = happyShift action_9
action_0 (58) = happyShift action_10
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (17) = happyGoto action_6
action_0 _ = happyFail

action_1 (63) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (66) = happyAccept
action_3 _ = happyFail

action_4 (50) = happyShift action_7
action_4 (54) = happyShift action_8
action_4 (56) = happyShift action_9
action_4 (58) = happyShift action_10
action_4 (8) = happyGoto action_4
action_4 (9) = happyGoto action_12
action_4 (17) = happyGoto action_6
action_4 _ = happyReduce_6

action_5 _ = happyReduce_4

action_6 (63) = happyShift action_2
action_6 (4) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_33

action_8 _ = happyReduce_31

action_9 _ = happyReduce_32

action_10 _ = happyReduce_34

action_11 (34) = happyShift action_13
action_11 _ = happyFail

action_12 _ = happyReduce_7

action_13 (50) = happyShift action_7
action_13 (54) = happyShift action_8
action_13 (56) = happyShift action_9
action_13 (58) = happyShift action_10
action_13 (10) = happyGoto action_14
action_13 (11) = happyGoto action_15
action_13 (17) = happyGoto action_16
action_13 _ = happyReduce_9

action_14 (39) = happyShift action_19
action_14 _ = happyReduce_10

action_15 (35) = happyShift action_18
action_15 _ = happyFail

action_16 (63) = happyShift action_2
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_8

action_18 (60) = happyShift action_22
action_18 (12) = happyGoto action_21
action_18 _ = happyFail

action_19 (50) = happyShift action_7
action_19 (54) = happyShift action_8
action_19 (56) = happyShift action_9
action_19 (58) = happyShift action_10
action_19 (10) = happyGoto action_14
action_19 (11) = happyGoto action_20
action_19 (17) = happyGoto action_16
action_19 _ = happyReduce_9

action_20 _ = happyReduce_11

action_21 _ = happyReduce_5

action_22 (13) = happyGoto action_23
action_22 _ = happyReduce_13

action_23 (30) = happyShift action_37
action_23 (34) = happyShift action_38
action_23 (40) = happyShift action_39
action_23 (43) = happyShift action_40
action_23 (50) = happyShift action_7
action_23 (52) = happyShift action_41
action_23 (53) = happyShift action_42
action_23 (54) = happyShift action_8
action_23 (55) = happyShift action_43
action_23 (56) = happyShift action_9
action_23 (57) = happyShift action_44
action_23 (58) = happyShift action_10
action_23 (59) = happyShift action_45
action_23 (60) = happyShift action_22
action_23 (62) = happyShift action_46
action_23 (63) = happyShift action_2
action_23 (64) = happyShift action_47
action_23 (65) = happyShift action_48
action_23 (4) = happyGoto action_24
action_23 (5) = happyGoto action_25
action_23 (6) = happyGoto action_26
action_23 (12) = happyGoto action_27
action_23 (14) = happyGoto action_28
action_23 (17) = happyGoto action_29
action_23 (19) = happyGoto action_30
action_23 (20) = happyGoto action_31
action_23 (21) = happyGoto action_32
action_23 (22) = happyGoto action_33
action_23 (23) = happyGoto action_34
action_23 (24) = happyGoto action_35
action_23 (25) = happyGoto action_36
action_23 _ = happyFail

action_24 (34) = happyShift action_77
action_24 (38) = happyShift action_78
action_24 (41) = happyShift action_79
action_24 (46) = happyShift action_80
action_24 _ = happyReduce_38

action_25 _ = happyReduce_39

action_26 _ = happyReduce_43

action_27 _ = happyReduce_16

action_28 _ = happyReduce_14

action_29 (63) = happyShift action_2
action_29 (4) = happyGoto action_74
action_29 (15) = happyGoto action_75
action_29 (16) = happyGoto action_76
action_29 _ = happyFail

action_30 _ = happyReduce_47

action_31 _ = happyReduce_49

action_32 (32) = happyShift action_71
action_32 (36) = happyShift action_72
action_32 (42) = happyShift action_73
action_32 (28) = happyGoto action_70
action_32 _ = happyReduce_51

action_33 (37) = happyShift action_68
action_33 (40) = happyShift action_69
action_33 (27) = happyGoto action_67
action_33 _ = happyReduce_53

action_34 (31) = happyShift action_60
action_34 (33) = happyShift action_61
action_34 (44) = happyShift action_62
action_34 (45) = happyShift action_63
action_34 (47) = happyShift action_64
action_34 (48) = happyShift action_65
action_34 (49) = happyShift action_66
action_34 (29) = happyGoto action_59
action_34 _ = happyReduce_55

action_35 (61) = happyShift action_58
action_35 _ = happyReduce_57

action_36 (43) = happyShift action_57
action_36 _ = happyFail

action_37 (34) = happyShift action_38
action_37 (52) = happyShift action_41
action_37 (57) = happyShift action_44
action_37 (63) = happyShift action_2
action_37 (64) = happyShift action_47
action_37 (65) = happyShift action_48
action_37 (4) = happyGoto action_50
action_37 (5) = happyGoto action_25
action_37 (6) = happyGoto action_26
action_37 (19) = happyGoto action_56
action_37 _ = happyFail

action_38 (30) = happyShift action_37
action_38 (34) = happyShift action_38
action_38 (40) = happyShift action_39
action_38 (52) = happyShift action_41
action_38 (57) = happyShift action_44
action_38 (63) = happyShift action_2
action_38 (64) = happyShift action_47
action_38 (65) = happyShift action_48
action_38 (4) = happyGoto action_50
action_38 (5) = happyGoto action_25
action_38 (6) = happyGoto action_26
action_38 (19) = happyGoto action_30
action_38 (20) = happyGoto action_31
action_38 (21) = happyGoto action_32
action_38 (22) = happyGoto action_33
action_38 (23) = happyGoto action_34
action_38 (24) = happyGoto action_35
action_38 (25) = happyGoto action_55
action_38 _ = happyFail

action_39 (34) = happyShift action_38
action_39 (52) = happyShift action_41
action_39 (57) = happyShift action_44
action_39 (63) = happyShift action_2
action_39 (64) = happyShift action_47
action_39 (65) = happyShift action_48
action_39 (4) = happyGoto action_50
action_39 (5) = happyGoto action_25
action_39 (6) = happyGoto action_26
action_39 (19) = happyGoto action_54
action_39 _ = happyFail

action_40 _ = happyReduce_15

action_41 _ = happyReduce_41

action_42 (34) = happyShift action_53
action_42 _ = happyFail

action_43 (30) = happyShift action_37
action_43 (34) = happyShift action_38
action_43 (40) = happyShift action_39
action_43 (43) = happyShift action_52
action_43 (52) = happyShift action_41
action_43 (57) = happyShift action_44
action_43 (63) = happyShift action_2
action_43 (64) = happyShift action_47
action_43 (65) = happyShift action_48
action_43 (4) = happyGoto action_50
action_43 (5) = happyGoto action_25
action_43 (6) = happyGoto action_26
action_43 (19) = happyGoto action_30
action_43 (20) = happyGoto action_31
action_43 (21) = happyGoto action_32
action_43 (22) = happyGoto action_33
action_43 (23) = happyGoto action_34
action_43 (24) = happyGoto action_35
action_43 (25) = happyGoto action_51
action_43 _ = happyFail

action_44 _ = happyReduce_40

action_45 (34) = happyShift action_49
action_45 _ = happyFail

action_46 _ = happyReduce_12

action_47 _ = happyReduce_2

action_48 _ = happyReduce_3

action_49 (30) = happyShift action_37
action_49 (34) = happyShift action_38
action_49 (40) = happyShift action_39
action_49 (52) = happyShift action_41
action_49 (57) = happyShift action_44
action_49 (63) = happyShift action_2
action_49 (64) = happyShift action_47
action_49 (65) = happyShift action_48
action_49 (4) = happyGoto action_50
action_49 (5) = happyGoto action_25
action_49 (6) = happyGoto action_26
action_49 (19) = happyGoto action_30
action_49 (20) = happyGoto action_31
action_49 (21) = happyGoto action_32
action_49 (22) = happyGoto action_33
action_49 (23) = happyGoto action_34
action_49 (24) = happyGoto action_35
action_49 (25) = happyGoto action_97
action_49 _ = happyFail

action_50 (34) = happyShift action_77
action_50 _ = happyReduce_38

action_51 (43) = happyShift action_96
action_51 _ = happyFail

action_52 _ = happyReduce_22

action_53 (30) = happyShift action_37
action_53 (34) = happyShift action_38
action_53 (40) = happyShift action_39
action_53 (52) = happyShift action_41
action_53 (57) = happyShift action_44
action_53 (63) = happyShift action_2
action_53 (64) = happyShift action_47
action_53 (65) = happyShift action_48
action_53 (4) = happyGoto action_50
action_53 (5) = happyGoto action_25
action_53 (6) = happyGoto action_26
action_53 (19) = happyGoto action_30
action_53 (20) = happyGoto action_31
action_53 (21) = happyGoto action_32
action_53 (22) = happyGoto action_33
action_53 (23) = happyGoto action_34
action_53 (24) = happyGoto action_35
action_53 (25) = happyGoto action_95
action_53 _ = happyFail

action_54 _ = happyReduce_45

action_55 (35) = happyShift action_94
action_55 _ = happyFail

action_56 _ = happyReduce_46

action_57 _ = happyReduce_26

action_58 (30) = happyShift action_37
action_58 (34) = happyShift action_38
action_58 (40) = happyShift action_39
action_58 (52) = happyShift action_41
action_58 (57) = happyShift action_44
action_58 (63) = happyShift action_2
action_58 (64) = happyShift action_47
action_58 (65) = happyShift action_48
action_58 (4) = happyGoto action_50
action_58 (5) = happyGoto action_25
action_58 (6) = happyGoto action_26
action_58 (19) = happyGoto action_30
action_58 (20) = happyGoto action_31
action_58 (21) = happyGoto action_32
action_58 (22) = happyGoto action_33
action_58 (23) = happyGoto action_34
action_58 (24) = happyGoto action_35
action_58 (25) = happyGoto action_93
action_58 _ = happyFail

action_59 (30) = happyShift action_37
action_59 (34) = happyShift action_38
action_59 (40) = happyShift action_39
action_59 (52) = happyShift action_41
action_59 (57) = happyShift action_44
action_59 (63) = happyShift action_2
action_59 (64) = happyShift action_47
action_59 (65) = happyShift action_48
action_59 (4) = happyGoto action_50
action_59 (5) = happyGoto action_25
action_59 (6) = happyGoto action_26
action_59 (19) = happyGoto action_30
action_59 (20) = happyGoto action_31
action_59 (21) = happyGoto action_32
action_59 (22) = happyGoto action_92
action_59 _ = happyFail

action_60 _ = happyReduce_71

action_61 (30) = happyShift action_37
action_61 (34) = happyShift action_38
action_61 (40) = happyShift action_39
action_61 (52) = happyShift action_41
action_61 (57) = happyShift action_44
action_61 (63) = happyShift action_2
action_61 (64) = happyShift action_47
action_61 (65) = happyShift action_48
action_61 (4) = happyGoto action_50
action_61 (5) = happyGoto action_25
action_61 (6) = happyGoto action_26
action_61 (19) = happyGoto action_30
action_61 (20) = happyGoto action_31
action_61 (21) = happyGoto action_32
action_61 (22) = happyGoto action_33
action_61 (23) = happyGoto action_34
action_61 (24) = happyGoto action_91
action_61 _ = happyFail

action_62 _ = happyReduce_66

action_63 _ = happyReduce_67

action_64 _ = happyReduce_70

action_65 _ = happyReduce_68

action_66 _ = happyReduce_69

action_67 (30) = happyShift action_37
action_67 (34) = happyShift action_38
action_67 (40) = happyShift action_39
action_67 (52) = happyShift action_41
action_67 (57) = happyShift action_44
action_67 (63) = happyShift action_2
action_67 (64) = happyShift action_47
action_67 (65) = happyShift action_48
action_67 (4) = happyGoto action_50
action_67 (5) = happyGoto action_25
action_67 (6) = happyGoto action_26
action_67 (19) = happyGoto action_30
action_67 (20) = happyGoto action_31
action_67 (21) = happyGoto action_90
action_67 _ = happyFail

action_68 _ = happyReduce_61

action_69 _ = happyReduce_62

action_70 (30) = happyShift action_37
action_70 (34) = happyShift action_38
action_70 (40) = happyShift action_39
action_70 (52) = happyShift action_41
action_70 (57) = happyShift action_44
action_70 (63) = happyShift action_2
action_70 (64) = happyShift action_47
action_70 (65) = happyShift action_48
action_70 (4) = happyGoto action_50
action_70 (5) = happyGoto action_25
action_70 (6) = happyGoto action_26
action_70 (19) = happyGoto action_30
action_70 (20) = happyGoto action_89
action_70 _ = happyFail

action_71 _ = happyReduce_65

action_72 _ = happyReduce_63

action_73 _ = happyReduce_64

action_74 (46) = happyShift action_88
action_74 _ = happyReduce_27

action_75 (39) = happyShift action_87
action_75 _ = happyReduce_29

action_76 (43) = happyShift action_86
action_76 _ = happyFail

action_77 (30) = happyShift action_37
action_77 (34) = happyShift action_38
action_77 (40) = happyShift action_39
action_77 (52) = happyShift action_41
action_77 (57) = happyShift action_44
action_77 (63) = happyShift action_2
action_77 (64) = happyShift action_47
action_77 (65) = happyShift action_48
action_77 (4) = happyGoto action_50
action_77 (5) = happyGoto action_25
action_77 (6) = happyGoto action_26
action_77 (19) = happyGoto action_30
action_77 (20) = happyGoto action_31
action_77 (21) = happyGoto action_32
action_77 (22) = happyGoto action_33
action_77 (23) = happyGoto action_34
action_77 (24) = happyGoto action_35
action_77 (25) = happyGoto action_84
action_77 (26) = happyGoto action_85
action_77 _ = happyReduce_58

action_78 (43) = happyShift action_83
action_78 _ = happyFail

action_79 (43) = happyShift action_82
action_79 _ = happyFail

action_80 (30) = happyShift action_37
action_80 (34) = happyShift action_38
action_80 (40) = happyShift action_39
action_80 (52) = happyShift action_41
action_80 (57) = happyShift action_44
action_80 (63) = happyShift action_2
action_80 (64) = happyShift action_47
action_80 (65) = happyShift action_48
action_80 (4) = happyGoto action_50
action_80 (5) = happyGoto action_25
action_80 (6) = happyGoto action_26
action_80 (19) = happyGoto action_30
action_80 (20) = happyGoto action_31
action_80 (21) = happyGoto action_32
action_80 (22) = happyGoto action_33
action_80 (23) = happyGoto action_34
action_80 (24) = happyGoto action_35
action_80 (25) = happyGoto action_81
action_80 _ = happyFail

action_81 (43) = happyShift action_104
action_81 _ = happyFail

action_82 _ = happyReduce_20

action_83 _ = happyReduce_19

action_84 (39) = happyShift action_103
action_84 _ = happyReduce_59

action_85 (35) = happyShift action_102
action_85 _ = happyFail

action_86 _ = happyReduce_17

action_87 (63) = happyShift action_2
action_87 (4) = happyGoto action_74
action_87 (15) = happyGoto action_75
action_87 (16) = happyGoto action_101
action_87 _ = happyFail

action_88 (30) = happyShift action_37
action_88 (34) = happyShift action_38
action_88 (40) = happyShift action_39
action_88 (52) = happyShift action_41
action_88 (57) = happyShift action_44
action_88 (63) = happyShift action_2
action_88 (64) = happyShift action_47
action_88 (65) = happyShift action_48
action_88 (4) = happyGoto action_50
action_88 (5) = happyGoto action_25
action_88 (6) = happyGoto action_26
action_88 (19) = happyGoto action_30
action_88 (20) = happyGoto action_31
action_88 (21) = happyGoto action_32
action_88 (22) = happyGoto action_33
action_88 (23) = happyGoto action_34
action_88 (24) = happyGoto action_35
action_88 (25) = happyGoto action_100
action_88 _ = happyFail

action_89 _ = happyReduce_48

action_90 (32) = happyShift action_71
action_90 (36) = happyShift action_72
action_90 (42) = happyShift action_73
action_90 (28) = happyGoto action_70
action_90 _ = happyReduce_50

action_91 _ = happyReduce_54

action_92 (37) = happyShift action_68
action_92 (40) = happyShift action_69
action_92 (27) = happyGoto action_67
action_92 _ = happyReduce_52

action_93 _ = happyReduce_56

action_94 _ = happyReduce_44

action_95 (35) = happyShift action_99
action_95 _ = happyFail

action_96 _ = happyReduce_21

action_97 (35) = happyShift action_98
action_97 _ = happyFail

action_98 (30) = happyShift action_37
action_98 (34) = happyShift action_38
action_98 (40) = happyShift action_39
action_98 (43) = happyShift action_40
action_98 (50) = happyShift action_7
action_98 (52) = happyShift action_41
action_98 (53) = happyShift action_42
action_98 (54) = happyShift action_8
action_98 (55) = happyShift action_43
action_98 (56) = happyShift action_9
action_98 (57) = happyShift action_44
action_98 (58) = happyShift action_10
action_98 (59) = happyShift action_45
action_98 (60) = happyShift action_22
action_98 (63) = happyShift action_2
action_98 (64) = happyShift action_47
action_98 (65) = happyShift action_48
action_98 (4) = happyGoto action_24
action_98 (5) = happyGoto action_25
action_98 (6) = happyGoto action_26
action_98 (12) = happyGoto action_27
action_98 (14) = happyGoto action_107
action_98 (17) = happyGoto action_29
action_98 (19) = happyGoto action_30
action_98 (20) = happyGoto action_31
action_98 (21) = happyGoto action_32
action_98 (22) = happyGoto action_33
action_98 (23) = happyGoto action_34
action_98 (24) = happyGoto action_35
action_98 (25) = happyGoto action_36
action_98 _ = happyFail

action_99 (30) = happyShift action_37
action_99 (34) = happyShift action_38
action_99 (40) = happyShift action_39
action_99 (43) = happyShift action_40
action_99 (50) = happyShift action_7
action_99 (52) = happyShift action_41
action_99 (53) = happyShift action_42
action_99 (54) = happyShift action_8
action_99 (55) = happyShift action_43
action_99 (56) = happyShift action_9
action_99 (57) = happyShift action_44
action_99 (58) = happyShift action_10
action_99 (59) = happyShift action_45
action_99 (60) = happyShift action_22
action_99 (63) = happyShift action_2
action_99 (64) = happyShift action_47
action_99 (65) = happyShift action_48
action_99 (4) = happyGoto action_24
action_99 (5) = happyGoto action_25
action_99 (6) = happyGoto action_26
action_99 (12) = happyGoto action_27
action_99 (14) = happyGoto action_106
action_99 (17) = happyGoto action_29
action_99 (19) = happyGoto action_30
action_99 (20) = happyGoto action_31
action_99 (21) = happyGoto action_32
action_99 (22) = happyGoto action_33
action_99 (23) = happyGoto action_34
action_99 (24) = happyGoto action_35
action_99 (25) = happyGoto action_36
action_99 _ = happyFail

action_100 _ = happyReduce_28

action_101 _ = happyReduce_30

action_102 _ = happyReduce_42

action_103 (30) = happyShift action_37
action_103 (34) = happyShift action_38
action_103 (40) = happyShift action_39
action_103 (52) = happyShift action_41
action_103 (57) = happyShift action_44
action_103 (63) = happyShift action_2
action_103 (64) = happyShift action_47
action_103 (65) = happyShift action_48
action_103 (4) = happyGoto action_50
action_103 (5) = happyGoto action_25
action_103 (6) = happyGoto action_26
action_103 (19) = happyGoto action_30
action_103 (20) = happyGoto action_31
action_103 (21) = happyGoto action_32
action_103 (22) = happyGoto action_33
action_103 (23) = happyGoto action_34
action_103 (24) = happyGoto action_35
action_103 (25) = happyGoto action_84
action_103 (26) = happyGoto action_105
action_103 _ = happyReduce_58

action_104 _ = happyReduce_18

action_105 _ = happyReduce_60

action_106 (51) = happyShift action_108
action_106 _ = happyReduce_23

action_107 _ = happyReduce_25

action_108 (30) = happyShift action_37
action_108 (34) = happyShift action_38
action_108 (40) = happyShift action_39
action_108 (43) = happyShift action_40
action_108 (50) = happyShift action_7
action_108 (52) = happyShift action_41
action_108 (53) = happyShift action_42
action_108 (54) = happyShift action_8
action_108 (55) = happyShift action_43
action_108 (56) = happyShift action_9
action_108 (57) = happyShift action_44
action_108 (58) = happyShift action_10
action_108 (59) = happyShift action_45
action_108 (60) = happyShift action_22
action_108 (63) = happyShift action_2
action_108 (64) = happyShift action_47
action_108 (65) = happyShift action_48
action_108 (4) = happyGoto action_24
action_108 (5) = happyGoto action_25
action_108 (6) = happyGoto action_26
action_108 (12) = happyGoto action_27
action_108 (14) = happyGoto action_109
action_108 (17) = happyGoto action_29
action_108 (19) = happyGoto action_30
action_108 (20) = happyGoto action_31
action_108 (21) = happyGoto action_32
action_108 (22) = happyGoto action_33
action_108 (23) = happyGoto action_34
action_108 (24) = happyGoto action_35
action_108 (25) = happyGoto action_36
action_108 _ = happyFail

action_109 _ = happyReduce_24

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (AbsLatte.Program happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 8 happyReduction_5
happyReduction_5 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AbsLatte.FnDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ((:[]) happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  9 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  10 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn10
		 (AbsLatte.Arg happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  11 happyReduction_9
happyReduction_9  =  HappyAbsSyn11
		 ([]
	)

happyReduce_10 = happySpecReduce_1  11 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ((:[]) happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  12 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (AbsLatte.Block (reverse happy_var_2)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  13 happyReduction_13
happyReduction_13  =  HappyAbsSyn13
		 ([]
	)

happyReduce_14 = happySpecReduce_2  13 happyReduction_14
happyReduction_14 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  14 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn14
		 (AbsLatte.Empty
	)

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsLatte.BStmt happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  14 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsLatte.Decl happy_var_1 happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 14 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AbsLatte.Ass happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsLatte.Incr happy_var_1
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsLatte.Decr happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (AbsLatte.Ret happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  14 happyReduction_22
happyReduction_22 _
	_
	 =  HappyAbsSyn14
		 (AbsLatte.VRet
	)

happyReduce_23 = happyReduce 5 14 happyReduction_23
happyReduction_23 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AbsLatte.Cond happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 7 14 happyReduction_24
happyReduction_24 ((HappyAbsSyn14  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AbsLatte.CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5 14 happyReduction_25
happyReduction_25 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (AbsLatte.While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn14
		 (AbsLatte.SExp happy_var_1
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  15 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsLatte.NoInit happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  15 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (AbsLatte.Init happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ((:[]) happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn17
		 (AbsLatte.Int
	)

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn17
		 (AbsLatte.Str
	)

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn17
		 (AbsLatte.Bool
	)

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn17
		 (AbsLatte.Void
	)

happyReduce_35 = happySpecReduce_0  18 happyReduction_35
happyReduction_35  =  HappyAbsSyn18
		 ([]
	)

happyReduce_36 = happySpecReduce_1  18 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ((:[]) happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  19 happyReduction_38
happyReduction_38 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.EVar happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  19 happyReduction_39
happyReduction_39 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.ELitInt happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  19 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn19
		 (AbsLatte.ELitTrue
	)

happyReduce_41 = happySpecReduce_1  19 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn19
		 (AbsLatte.ELitFalse
	)

happyReduce_42 = happyReduce 4 19 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (AbsLatte.EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  19 happyReduction_43
happyReduction_43 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.EString happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  19 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  20 happyReduction_45
happyReduction_45 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (AbsLatte.Neg happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  20 happyReduction_46
happyReduction_46 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (AbsLatte.Not happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  20 happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  21 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  21 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  22 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  22 happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  23 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  23 happyReduction_53
happyReduction_53 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  24 happyReduction_54
happyReduction_54 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.EAnd happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (AbsLatte.EOr happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  26 happyReduction_58
happyReduction_58  =  HappyAbsSyn26
		 ([]
	)

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn26
		 ((:[]) happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  26 happyReduction_60
happyReduction_60 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn26
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  27 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn27
		 (AbsLatte.Plus
	)

happyReduce_62 = happySpecReduce_1  27 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn27
		 (AbsLatte.Minus
	)

happyReduce_63 = happySpecReduce_1  28 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn28
		 (AbsLatte.Times
	)

happyReduce_64 = happySpecReduce_1  28 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn28
		 (AbsLatte.Div
	)

happyReduce_65 = happySpecReduce_1  28 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn28
		 (AbsLatte.Mod
	)

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn29
		 (AbsLatte.LTH
	)

happyReduce_67 = happySpecReduce_1  29 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn29
		 (AbsLatte.LE
	)

happyReduce_68 = happySpecReduce_1  29 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn29
		 (AbsLatte.GTH
	)

happyReduce_69 = happySpecReduce_1  29 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn29
		 (AbsLatte.GE
	)

happyReduce_70 = happySpecReduce_1  29 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn29
		 (AbsLatte.EQU
	)

happyReduce_71 = happySpecReduce_1  29 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn29
		 (AbsLatte.NE
	)

happyNewToken action sts stk [] =
	action 66 66 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 30;
	PT _ (TS _ 2) -> cont 31;
	PT _ (TS _ 3) -> cont 32;
	PT _ (TS _ 4) -> cont 33;
	PT _ (TS _ 5) -> cont 34;
	PT _ (TS _ 6) -> cont 35;
	PT _ (TS _ 7) -> cont 36;
	PT _ (TS _ 8) -> cont 37;
	PT _ (TS _ 9) -> cont 38;
	PT _ (TS _ 10) -> cont 39;
	PT _ (TS _ 11) -> cont 40;
	PT _ (TS _ 12) -> cont 41;
	PT _ (TS _ 13) -> cont 42;
	PT _ (TS _ 14) -> cont 43;
	PT _ (TS _ 15) -> cont 44;
	PT _ (TS _ 16) -> cont 45;
	PT _ (TS _ 17) -> cont 46;
	PT _ (TS _ 18) -> cont 47;
	PT _ (TS _ 19) -> cont 48;
	PT _ (TS _ 20) -> cont 49;
	PT _ (TS _ 21) -> cont 50;
	PT _ (TS _ 22) -> cont 51;
	PT _ (TS _ 23) -> cont 52;
	PT _ (TS _ 24) -> cont 53;
	PT _ (TS _ 25) -> cont 54;
	PT _ (TS _ 26) -> cont 55;
	PT _ (TS _ 27) -> cont 56;
	PT _ (TS _ 28) -> cont 57;
	PT _ (TS _ 29) -> cont 58;
	PT _ (TS _ 30) -> cont 59;
	PT _ (TS _ 31) -> cont 60;
	PT _ (TS _ 32) -> cont 61;
	PT _ (TS _ 33) -> cont 62;
	PT _ (TV happy_dollar_dollar) -> cont 63;
	PT _ (TI happy_dollar_dollar) -> cont 64;
	PT _ (TL happy_dollar_dollar) -> cont 65;
	_ -> happyError' (tk:tks)
	}

happyError_ 66 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib64/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghca582_0/ghc_2.h" #-}












































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
