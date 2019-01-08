declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)


define i32 @main() {
.entry.0:
    %abcABC000___.0 = alloca i32
    store i32 0, i32* %abcABC000___.0
    %0 = load i32, i32* %abcABC000___.0
    ret i32 %0
}

