import sys, os, re, shutil

if __name__ == "__main__":
    print("Python utility to transform the JavaScript Parser and Lexer grammars to work with a python output")
    if len(sys.argv) == 1:
        print(f"""Please run this script as {sys.argv[0]} <grammar>.g4""")
        sys.exit(0)
    file_path = sys.argv[1]
    if not os.path.exists(file_path):
        print(f"Could not find file: {file_path}")
        sys.exit(1)
    parts = os.path.split(file_path)
    file_name = parts[-1]

    if file_name.lower() not in ["javascriptparser.g4", "javascriptlexer.g4"]:
        print("Could not determine which grammar you have specified, please specify either the Paarser or the Lexer")
        sys.exit(1)

    shutil.move(file_path, file_path + ".bak")
    input_file = open(file_path + ".bak",'r')
    output_file = open(file_path, 'w')
    if file_name.lower() == "javascriptparser.g4":
        for x in input_file:
            if 'this.' in x:
                x = x.replace('this.', 'self.')
            output_file.write(x)
            output_file.flush()
    elif file_name.lower() == "javascriptlexer.g4":
        for x in input_file:
            if '!this.IsStrictMode' in x:
                x = x.replace('!this.Is', 'not self.is')
            if 'this.Is' in x:
                x = x.replace('this.Is', 'self.is')
            if 'this.ProcessOpenBrace();' in x:
                x = x.replace('this.ProcessOpenBrace();', 'self.processOpenBrace()')
            if 'this.ProcessCloseBrace();' in x:
                x = x.replace('this.ProcessCloseBrace();', 'self.processCloseBrace()')
            if 'this.ProcessStringLiteral();' in x:
                x = x.replace('this.ProcessStringLiteral();', 'self.processStringLiteral()')
            output_file.write(x)
            output_file.flush()

    input_file.close()
    output_file.close()
