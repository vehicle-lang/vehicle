# Vehicle 错误示例文件集

本文件夹包含多个 Vehicle 语言的错误示例，用于测试和展示 Vehicle 的错误报告功能，特别是JSON格式的错误输出。

## 错误文件说明

### 语法错误

1. **01_syntax_error_missing_semicolon.vcl**
   - 错误：缺少分号
   - 正确写法：在语句末尾添加分号
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/01_syntax_error_missing_semicolon.vcl`

2. **02_syntax_error_unfinished_expr.vcl**
   - 错误：表达式未完成
   - 正确写法：完成表达式
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/02_syntax_error_unfinished_expr.vcl`

3. **03_syntax_error_unbalanced_paren.vcl**
   - 错误：括号不匹配
   - 正确写法：添加缺失的括号
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/03_syntax_error_unbalanced_paren.vcl`

### 引用错误

4. **04_circular_reference.vcl**
   - 错误：循环引用
   - 正确写法：解除循环依赖
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/04_circular_reference.vcl`

### 函数错误

5. **05_function_redefinition.vcl**
   - 错误：函数重定义
   - 正确写法：确保函数名唯一
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/05_function_redefinition.vcl`

### 类型错误

6. **06_type_mismatch.vcl**
   - 错误：类型不匹配
   - 正确写法：使用正确的类型
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/06_type_mismatch.vcl`

7. **07_missing_arguments.vcl**
   - 错误：缺少参数
   - 正确写法：提供所有必需参数
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/07_missing_arguments.vcl`

8. **08_too_many_arguments.vcl**
   - 错误：参数过多
   - 正确写法：只提供所需参数
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/08_too_many_arguments.vcl`

9. **09_undefined_type.vcl**
   - 错误：未定义类型
   - 正确写法：使用已定义的类型或定义新类型
   - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/09_undefined_type.vcl`

10. **10_invalid_annotation.vcl**
    - 错误：无效注解
    - 正确写法：使用正确的注解格式
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/10_invalid_annotation.vcl`

11. **11_missing_type_declaration.vcl**
    - 错误：缺少类型声明
    - 正确写法：添加必要的类型声明
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/11_missing_type_declaration.vcl`

12. **12_inconsistent_vector.vcl**
    - 错误：向量元素类型不一致
    - 正确写法：使用相同类型的元素
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/12_inconsistent_vector.vcl`

13. **13_float_as_int.vcl**
    - 错误：将浮点数用作整数
    - 正确写法：使用正确的数值类型
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/13_float_as_int.vcl`

14. **14_property_not_bool.vcl**
    - 错误：属性不是布尔值
    - 正确写法：确保属性为布尔类型
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/14_property_not_bool.vcl`

15. **15_function_type_mismatch.vcl**
    - 错误：函数类型不匹配
    - 正确写法：修正函数类型或使用方式
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/15_function_type_mismatch.vcl`

16. **16_wrong_argument_type.vcl**
    - 错误：参数类型错误
    - 正确写法：使用正确类型的参数
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/16_wrong_argument_type.vcl`

17. **17_vector_size_mismatch.vcl**
    - 错误：向量大小不匹配
    - 正确写法：使用相同大小的向量
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/17_vector_size_mismatch.vcl`

### 其他错误

18. **18_invalid_tokens.vcl**
    - 错误：无效的标记
    - 正确写法：使用有效的语法标记
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/18_invalid_tokens.vcl`

19. **19_duplicate_definition.vcl**
    - 错误：重复定义
    - 正确写法：确保每个标识符只定义一次
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/19_duplicate_definition.vcl`

20. **20_custom_type_mismatch.vcl**
    - 错误：自定义类型不匹配
    - 正确写法：遵循自定义类型的定义
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/20_custom_type_mismatch.vcl`

21. **21_type_error_operation.vcl**
    - 错误：操作类型错误
    - 正确写法：使用适合该操作的类型
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/21_type_error_operation.vcl`

22. **22_invalid_index.vcl**
    - 错误：无效索引
    - 正确写法：使用有效的索引值
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/22_invalid_index.vcl`

23. **23_duplicate_annotation.vcl**
    - 错误：重复注解
    - 正确写法：避免重复的注解
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/23_duplicate_annotation.vcl`

24. **24_index_out_of_bounds.vcl**
    - 错误：索引越界
    - 正确写法：使用范围内的索引
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/24_index_out_of_bounds.vcl`

25. **25_non_numeric_index.vcl**
    - 错误：非数字索引
    - 正确写法：使用数字索引
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/25_non_numeric_index.vcl`

26. **26_invalid_quantifier.vcl**
    - 错误：无效的量词
    - 正确写法：使用有效的量词语法
    - 调用方式：`cabal exec vehicle -- --json-error check -s error_examples/26_invalid_quantifier.vcl`

## 使用方法

要查看单个错误文件的JSON错误输出，使用以下命令格式：

```bash
cabal exec vehicle -- --json-error check -s error_examples/文件名.vcl
```

如需格式化JSON输出以便于阅读，可以使用：

```bash
cabal exec vehicle -- --json-error check -s error_examples/文件名.vcl 2>/tmp/error.json && cat /tmp/error.json | python3 -m json.tool
```

## 错误输出解析

JSON错误输出包含以下主要字段：
- `error`: 完整的错误消息，包含文件名、位置和详细错误描述
- `provenance`: 错误位置信息，包含以下子字段：
  - `file`: 文件名
  - `range`: 错误范围，包含 `start` 和 `end` 位置，每个位置包含 `line` 和 `column` 值

示例输出：
```json
{
  "error": "Error in file 'test.vcl' at Line 1, Columns 18-19: the value '5' is too big to be used as an index of size '3'.",
  "provenance": {
    "file": "test.vcl",
    "range": {
      "end": {
        "column": 19,
        "line": 1
      },
      "start": {
        "column": 18,
        "line": 1
      }
    }
  }
}
```

通过分析这些JSON输出，可以评估错误报告的质量和有用性。 