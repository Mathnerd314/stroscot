; ModuleID = 'hash.c'
source_filename = "hash.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: alwaysinline nounwind sspstrong uwtable
define ghccc void @test2(i64* noalias, i64* noalias, i64* noalias, i64, i64, i64, i64, i64, i64, i64) local_unnamed_addr #0 {
  %11 = shl i64 %3, 1
  %12 = mul i64 %3, %3
  %13 = bitcast i64* %1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)**
  %14 = load void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*, void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)** %13, align 8, !tbaa !3
  tail call ghccc void %14(i64* %0, i64* %1, i64* %2, i64 %11, i64 %12, i64 undef, i64 undef, i64 undef, i64 undef, i64 %9) #1
  ret void
}

attributes #0 = { alwaysinline nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 9.0.1 "}
!3 = !{!4, !4, i64 0}
!4 = !{!"long", !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
