declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.b.1 = internal constant [5 x i8] c"true\00"
@.str.b.2 = internal constant [6 x i8] c"false\00"

define i32 @main() {
.entry.0:
    br label %.land.left.expr.1
.land.left.expr.1:
    %0 = call i1 @t(i32 1)
    %1 = icmp eq i1 %0, 1
    br i1 %1, label %.land.right.expr.2, label %.land.after.3
.land.right.expr.2:
    %2 = call i1 @f(i32 2)
    br label %.land.after.3
.land.after.3:
    %3 = phi i1 [ 0, %.land.left.expr.1 ], [ %2, %.land.right.expr.2 ]
    call void @b(i1 %3)
    br label %.land.left.expr.4
.land.left.expr.4:
    %4 = call i1 @t(i32 3)
    %5 = icmp eq i1 %4, 1
    br i1 %5, label %.land.right.expr.5, label %.land.after.6
.land.right.expr.5:
    %6 = call i1 @t(i32 4)
    br label %.land.after.6
.land.after.6:
    %7 = phi i1 [ 0, %.land.left.expr.4 ], [ %6, %.land.right.expr.5 ]
    call void @b(i1 %7)
    br label %.lor.left.expr.7
.lor.left.expr.7:
    %8 = call i1 @t(i32 5)
    %9 = icmp eq i1 %8, 1
    br i1 %9, label %.lor.after.9, label %.lor.right.expr.8
.lor.right.expr.8:
    %10 = call i1 @t(i32 6)
    br label %.lor.after.9
.lor.after.9:
    %11 = phi i1 [ 1, %.lor.left.expr.7 ], [ %10, %.lor.right.expr.8 ]
    call void @b(i1 %11)
    br label %.land.left.expr.10
.land.left.expr.10:
    %12 = call i1 @f(i32 7)
    %13 = icmp eq i1 %12, 1
    br i1 %13, label %.land.right.expr.11, label %.land.after.12
.land.right.expr.11:
    %14 = call i1 @t(i32 8)
    br label %.land.after.12
.land.after.12:
    %15 = phi i1 [ 0, %.land.left.expr.10 ], [ %14, %.land.right.expr.11 ]
    call void @b(i1 %15)
    br label %.land.left.expr.13
.land.left.expr.13:
    %16 = call i1 @t(i32 9)
    %17 = icmp eq i1 %16, 1
    br i1 %17, label %.land.right.expr.14, label %.land.after.18
.land.right.expr.14:
    br label %.land.left.expr.15
.land.left.expr.15:
    %18 = call i1 @t(i32 10)
    %19 = icmp eq i1 %18, 1
    br i1 %19, label %.land.right.expr.16, label %.land.after.17
.land.right.expr.16:
    %20 = call i1 @t(i32 11)
    br label %.land.after.17
.land.after.17:
    %21 = phi i1 [ 0, %.land.left.expr.15 ], [ %20, %.land.right.expr.16 ]
    br label %.land.after.18
.land.after.18:
    %22 = phi i1 [ 0, %.land.left.expr.13 ], [ %21, %.land.after.17 ]
    call void @b(i1 %22)
    br label %.lor.left.expr.19
.lor.left.expr.19:
    %23 = call i1 @f(i32 12)
    %24 = icmp eq i1 %23, 1
    br i1 %24, label %.lor.after.24, label %.lor.right.expr.20
.lor.right.expr.20:
    br label %.land.left.expr.21
.land.left.expr.21:
    %25 = call i1 @f(i32 13)
    %26 = icmp eq i1 %25, 1
    br i1 %26, label %.land.right.expr.22, label %.land.after.23
.land.right.expr.22:
    %27 = call i1 @t(i32 14)
    br label %.land.after.23
.land.after.23:
    %28 = phi i1 [ 0, %.land.left.expr.21 ], [ %27, %.land.right.expr.22 ]
    br label %.lor.after.24
.lor.after.24:
    %29 = phi i1 [ 1, %.lor.left.expr.19 ], [ %28, %.land.after.23 ]
    call void @b(i1 %29)
    ret i32 0
}

define i1 @f(i32 %a) {
.entry.0:
%a.0 = alloca i32
store i32 %a, i32* %a.0
    %0 = load i32, i32* %a.0
    call void @printInt(i32 %0)
    ret i1 0
}

define i1 @t(i32 %a) {
.entry.0:
%a.0 = alloca i32
store i32 %a, i32* %a.0
    %0 = load i32, i32* %a.0
    %1 = call i1 @f(i32 %0)
    %2 = sub i1 1, %1
    ret i1 %2
}

define void @b(i1 %a) {
.entry.0:
%a.0 = alloca i1
store i1 %a, i1* %a.0
    br label %.if.cond.1
.if.cond.1:
    %0 = load i1, i1* %a.0
    br i1 %0, label %.if.true.2, label %.if.false.3
.if.true.2:
    %1 = getelementptr [5 x i8], [5 x i8]* @.str.b.1, i32 0, i32 0
    call void @printString(i8* %1)
    br label %.after.cond.4
.if.false.3:
    %2 = getelementptr [6 x i8], [6 x i8]* @.str.b.2, i32 0, i32 0
    call void @printString(i8* %2)
    br label %.after.cond.4
.after.cond.4:
    ret void
}

