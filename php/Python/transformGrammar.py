import sys, os, re, shutil

# This script was originally found in the directory of the Golang Grammar, used for the transformation of the Golang Parser to be compatible with Python Runtime
# It was copied and modified to handle the PHP Grammar transformation.
def main(argv):
    # Only the PhpLexer.g4 file needs to be transformed. 
    # However, the script also checks the PhpParser in case of a future change that may render it incompatible 
    fix("PhpLexer.g4")
    fix("PhpParser.g4")


def fix(file_path):
    print("Altering " + file_path)
    if not os.path.exists(file_path):
        print(f"Could not find file: {file_path}")
        sys.exit(1)
    parts = os.path.split(file_path)
    file_name = parts[-1]

    if file_name.lower() not in ["phpparser.g4", "phplexer.g4"]:
        print("Could not determine which grammar you have specified, please specify either the Parser or the Lexer")
        sys.exit(1)

    shutil.move(file_path, file_path + ".bak")
    input_file = open(file_path + ".bak",'r')
    output_file = open(file_path, 'w')
    for x in input_file:
        if 'this.' in x:
            x = x.replace('this.', 'self.')
        output_file.write(x)
        output_file.flush()

    print("Writing ...")
    input_file.close()
    output_file.close()


if __name__ == '__main__':
    main(sys.argv)


