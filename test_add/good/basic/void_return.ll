declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)


define i32 @main() {
.entry.0:
    call void @run()
    ret i32 0
}

define void @run() {
.entry.0:
    call void @printInt(i32 0)
    ret void
}

