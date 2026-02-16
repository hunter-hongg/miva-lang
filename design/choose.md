# MVP choose表达式

## 语法
```mvp 
choose (var) {
  when (val1) {
    ...
  }
  when (val2) {
    ...
  }
  otherwise { // 强制
    ...
  }
}
```

## 实现
- 转为`C++`的`if-else`表达式（多分支）
