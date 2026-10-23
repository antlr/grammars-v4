import re

def rename(path, replacements):
    """Apply a sequence of (old, new) renames to a file, using placeholders
    to prevent double-substitution of already-correct names."""
    with open(path, 'r', encoding='utf-8') as f:
        content = f.read()
    # First pass: protect already-correct names with placeholders
    for i, (old, new) in enumerate(replacements):
        content = content.replace(new, f'__PH{i}__')
    # Second pass: rename old -> new (via placeholder)
    for i, (old, new) in enumerate(replacements):
        content = content.replace(old, f'__PH{i}__')
    # Third pass: resolve all placeholders to their final names
    for i, (old, new) in enumerate(replacements):
        content = content.replace(f'__PH{i}__', new)
    with open(path, 'w', encoding='utf-8') as f:
        f.write(content)

rename('CLexerBase.cs', [
    ('CLexerBase', 'TrapCLexerBase'),
])

rename('CParserBase.cs', [
    ('CParserBase', 'TrapCParserBase'),
    ('CParser',     'TrapCParser'),
    ('CLexer',      'TrapCLexer'),
])

rename('ErrorListener.cs', [
    ('CLexer', 'TrapCLexer'),
])
