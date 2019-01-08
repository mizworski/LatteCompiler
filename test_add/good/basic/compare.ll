declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.main.1 = internal constant [2 x i8] c"4\00"
@.str.main.3 = internal constant [2 x i8] c"4\00"
@.str.main.5 = internal constant [2 x i8] c"5\00"
@.str.main.7 = internal constant [2 x i8] c"5\00"
@.str.main.9 = internal constant [2 x i8] c"6\00"
@.str.main.11 = internal constant [2 x i8] c"6\00"
@.str.main.13 = internal constant [2 x i8] c"7\00"
@.str.main.15 = internal constant [2 x i8] c"7\00"

define i32 @main() {
.entry.0:
    br label %.if.cond.1
.if.cond.1:
    %0 = icmp sle i32 1, 1
    br i1 %0, label %.if.true.2, label %.after.cond.3
.if.true.2:
    %1 = getelementptr [2 x i8], [2 x i8]* @.str.main.1, i32 0, i32 0
    call void @printString(i8* %1)
    br label %.after.cond.3
.after.cond.3:
    br label %.if.cond.4
.if.cond.4:
    %2 = icmp sge i32 1, 1
    br i1 %2, label %.if.true.5, label %.after.cond.6
.if.true.5:
    %3 = getelementptr [2 x i8], [2 x i8]* @.str.main.3, i32 0, i32 0
    call void @printString(i8* %3)
    br label %.after.cond.6
.after.cond.6:
    br label %.if.cond.7
.if.cond.7:
    %4 = icmp sgt i32 1, 1
    br i1 %4, label %.if.true.8, label %.after.cond.9
.if.true.8:
    %5 = getelementptr [2 x i8], [2 x i8]* @.str.main.5, i32 0, i32 0
    call void @printString(i8* %5)
    br label %.after.cond.9
.after.cond.9:
    br label %.if.cond.10
.if.cond.10:
    %6 = icmp slt i32 1, 1
    br i1 %6, label %.if.true.11, label %.after.cond.12
.if.true.11:
    %7 = getelementptr [2 x i8], [2 x i8]* @.str.main.7, i32 0, i32 0
    call void @printString(i8* %7)
    br label %.after.cond.12
.after.cond.12:
    br label %.if.cond.13
.if.cond.13:
    %8 = icmp slt i32 1, 2
    br i1 %8, label %.if.true.14, label %.after.cond.15
.if.true.14:
    %9 = getelementptr [2 x i8], [2 x i8]* @.str.main.9, i32 0, i32 0
    call void @printString(i8* %9)
    br label %.after.cond.15
.after.cond.15:
    br label %.if.cond.16
.if.cond.16:
    %10 = icmp sgt i32 2, 1
    br i1 %10, label %.if.true.17, label %.after.cond.18
.if.true.17:
    %11 = getelementptr [2 x i8], [2 x i8]* @.str.main.11, i32 0, i32 0
    call void @printString(i8* %11)
    br label %.after.cond.18
.after.cond.18:
    br label %.if.cond.19
.if.cond.19:
    %12 = icmp sgt i32 1, 2
    br i1 %12, label %.if.true.20, label %.after.cond.21
.if.true.20:
    %13 = getelementptr [2 x i8], [2 x i8]* @.str.main.13, i32 0, i32 0
    call void @printString(i8* %13)
    br label %.after.cond.21
.after.cond.21:
    br label %.if.cond.22
.if.cond.22:
    %14 = icmp slt i32 2, 1
    br i1 %14, label %.if.true.23, label %.after.cond.24
.if.true.23:
    %15 = getelementptr [2 x i8], [2 x i8]* @.str.main.15, i32 0, i32 0
    call void @printString(i8* %15)
    br label %.after.cond.24
.after.cond.24:
    ret i32 0
}

