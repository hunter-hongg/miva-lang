# MVP 不安全模型

## 基本法则
- 一个函数若调用了`unsafe`函数，必须声明其自身为`unsafe`函数
- 一个函数若调用了`unsafe`函数且作者确认**内存安全**，须声明自身为`trusted`函数
- 一个未声明`unsafe`的函数不得调用`unsafe`函数，但可以调用`trusted`函数

## 实现方案
- Parser阶段把`unsafe`和`trusted`标记为特殊函数
  - `unsafe foo = 函数体`解析为`DFuncUnsafe` 
  - `trusted foo = 函数体`解析为`DFuncTrusted`
- Symbol阶段，生成特殊符号表
- Semantic阶段，不对`DFuncUnsafe`和`DFuncTrusted`进行任何检查，包括所有权检查
- Semantic阶段在检查时（特殊**函数已经忽略**），禁止调用`DFuncUnsafe`（检索符号表）
- Codegen阶段，`DFuncUnsafe`和`DFuncTrusted`与普通函数生成方法相同，也要自动插入`return`等等
