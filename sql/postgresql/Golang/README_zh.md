# Antlr v4 Postgresql Golang

[English](./README.md)



使用步骤： 



# Step 1.   

更新词法文件和语法文件为最新版本，最新版本可以从这个地方获取：[https://github.com/antlr/grammars-v4/tree/master/sql/postgresql](https://github.com/antlr/grammars-v4/tree/master/sql/postgresql)



# Step 2. 

先运行项目根目录下的`main.go`，将`PostgreSQLLexer.g4`和`PostgreSQLParser.g4`转换为适合`Golang`使用的形式 ，转换后的目录存储在其它目录，默认存储在项目根目录下的Golang目录。 

### 为什么要做这一层转换？ 

原始的g4文件是兼容Java和C#的，但是Java和C#的某些特性在Golang中是无法适配的，比如在Java中的类的成员方法中可以省略this来调用同一个类或者父类的方法，但是在Golang中就不行，必须得手动指定receiver，其它的还有一些golang中的包路径之类的问题是与其它语言有差异的，但是又不想对原始的g4文件进行改动，因为一旦改动g4就无法保证Java和C#的正确性了，想想要在同一个g4文件中同时保证对这么多种语言的支持就很有压力，所以选择了基于原有文件根据golang的特性对其进行调整生成新的文件，以使其能够适应Golang的语法 



# Step 3. 

然后使用`antlr-4.10.1-complete.jar`从转换之后的词法文件和语法文件生成对应的代码，jar包的名字不同版本可能不同，请自行替换为自己的版本。  



生成词法解析代码：  

```bash
java -jar ./antlr-4.10.1-complete.jar -Dlanguage=Go ./Golang/PostgreSQLLexer.g4 -o parser -visitor
```



生成语法解析代码： 

```
java -jar ./antlr-4.10.1-complete.jar -Dlanguage=Go ./Golang/PostgreSQLParser.g4 -o parser -visitor 
```



# Step 4. 

运行`test/main.go`，康康是否能够正常输出。

