declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
declare void @error()
declare i8* @__concat(i8*, i8*)

@.str.main.0 = internal constant [9 x i8] c"\5Ca\5Cn\0A\09b\22\00"

define i32 @main() {
.entry.0:
    %0 = getelementptr [9 x i8], [9 x i8]* @.str.main.0, i32 0, i32 0
    call void @printString(i8* %0)
    ret i32 0
}

