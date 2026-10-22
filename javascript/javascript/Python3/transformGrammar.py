import sys, os, re, shutil

def main(argv):
    fix("JavaScriptLexer.g4")
    fix("JavaScriptParser.g4")

def fix(file_path):
    print("Altering " + file_path)
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
            if '!this.' in x:
                x = x.replace('!this.', 'not self.')
            if 'this.' in x:
                x = x.replace('this.', 'self.')
            output_file.write(x)
            output_file.flush()

    print("Writing ...")
    input_file.close()
    output_file.close()

if __name__ == '__main__':
    main(sys.argv)
