# MVP模块化-基于文件的模块化

## 1. 基础概念
- 模块: MVP项目中的一个**文件**，可能依赖**其他模块** 
- 包: MVP项目，包含**多个文件**，根目录有`mvp.toml`文件

## 2. 模块声明
```mvp
module std.io // 声明模块名，此处以std/io.mvp为例 

foo = () {
  ...
} // 定义函数，不导出

bar = () {
  ...
} // 也不导出 

iostruct = struct {
  ...
} // 还不导出 

export bar // 导出bar
export iostruct // 导出iostruct 
```

## 3. 模块使用
```mvp 
import "std/io" // 导入模块，查找 $MVP_STD/std/io.mvp 
import "myproject/myxxx/io" // 若包名为myproject，查找 包根/src/myxxx/io.mvp 
import "std/term" as term // 定义别名，导入std/term.mvp 

main = () {
  std.io.bar() // 调用bar，注意module std.io后调用方法 
  std.term.red("Hello, world!") // 调用std/term.mvp里export的red函数 
  term.red("Hello, World!") // 由于定义了别名，可以通过别名调用
}
```

## 4. 内部实现
- `foo.io`在C++层映射为`foo::io` 
- 注意`std`库特殊处理，映射为`mvp_std::io`
- `foo.io.print`映射为`foo::io::print` 
- `export`的函数和结构体**自动在头文件中声明**
- `import "..."`查找后即引入对应头文件
