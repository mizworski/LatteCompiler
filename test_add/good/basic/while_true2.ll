declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.main.3 = internal constant [12 x i8] c"jeszcze raz\00"

define i32 @main() {
.entry.0:
    br label %.loop.body.1
.loop.body.1:
    %x.1 = alloca i32
    store i32 0, i32* %x.1
    %0 = call i32 @readInt()
    store i32 %0, i32* %x.1
    br label %.if.cond.2
.if.cond.2:
    %1 = load i32, i32* %x.1
    %2 = icmp eq i32 %1, 1
    br i1 %2, label %.if.true.3, label %.if.false.4
.if.true.3:
    ret i32 0
.if.false.4:
    %3 = getelementptr [12 x i8], [12 x i8]* @.str.main.3, i32 0, i32 0
    call void @printString(i8* %3)
    br label %.after.cond.5
.after.cond.5:
    br label %.loop.body.1
}

