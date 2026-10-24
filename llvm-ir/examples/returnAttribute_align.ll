define void @main() {
  %0 = call nonnull align 8 i32 @f()
  ret void
}