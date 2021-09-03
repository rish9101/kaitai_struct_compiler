# Kaitai - Compiler

## How it works


JavaMain.scala - The main class for JVM based run.

Takes the command line arguments, processes them to a Config class `CLIConfig`
Then calls `run` function in `class JavaMain` - which calls `compileOneInput`

We take the yaml path and pass it to the `JavaKSYParser`

Actual parsing begins now.

First the entire YAML file is parsed into a scala `map` structure. with keys being attributes and values being values.
In JavaKSYParser object, `fileNameToSpec` function does this. This map is called `firstSpec`

Now `firstSpec` is parsed into `JavaClassSpec` object tree.

What is ClassSpec?
`ClassSpec` represent the different things related to a type in kaitai struct. e.g for every type, it records the sequence, the subtypes, the enums, instances etc etc.
`ClassSpec.fromYaml` parses the topmost YAML class and generates the subsequently parses all the types and seq into respective specs (ClassSpecs, AttrSpecs, MetaSpecs etc)


we have the `ClassSpecs` object - Which contains different `ClassSpec`, 
|
|-> Back to `JavaMain.scala` with `compileOneLang` to be called.
    |
    |-> `compileOneLang` gets the `LanguageCompilerStatic/byString` method to get the target language specific static compiler
        |
        |-> `JavaMain/compileSpecAndWriteToFile` called with the lang compiler and the specs
            |
            |->This calls the `Main.compile` method in Main.scala. First the ClassCompiler object created which instantiates the       required lang compiler (stored in the lang member of the object). Now `ClassCompiler/ClassCompiler.compile` method called.
                |
                |-> Actual definite Compilation starts (Documented only for python)- 
                    1.   First the header of the file is written. (imports etc)
                    2.   Now the opaque classes are compiled.
                    3.   Now `compileClass` method is called (in the same ClassCompiler function) - This compiles a single spec - Most important - How read method is generated - 
                        For python. - First `attrParse` method is called(defined in commonReads.scala) which first decides if the spec being parsed is an instance or not (for determining io). and then based on the endianNess parses the spec.
                        |-> Calls `attrParse0` emthod (in CommonReads) for this, which based on the spec repeat condition calls the `attrParse2` method (in EveryReadIsExpression) 
                            |
                            |-> `attrParse2` based on the dataType of spec, parses it (calling relevant writing functions   from the language specific compiler)


```python

class AuthInput(KaitaiStruct, GrammarInput):

    def __init__():
        self._fields_init()
    
    def _fields_init():
        self._fields = []
        self.send_uname = self.UnamePacket()
    
        self.constraints = dict()
        self.constrains[self.send_uname] = {'username': 'admin'}
        self.constraints[self.challenge] = {'solution': eval'()'}

    def __iter__():
        prev_data = []
        for f in fields:
            if transmit:
                yield TransmitInteraction(f.generate())
            else:
                temp = RecvInteraction()
                yield temp
                prev_data = prev_data + temp.data
                f.parse(prev_data)
                prev_data = prev_data[length_parsed:end]
            

```