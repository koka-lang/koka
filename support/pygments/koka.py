#------------------------------------------------------------------------------
# Copyright 2012 Microsoft Corporation.
#
# This is free software; you can redistribute it and/or modify it under the
# terms of the Apache License, Version 2.0. A copy of the License can be
# found in the file "license.txt" at the root of this distribution.
#------------------------------------------------------------------------------

# This is a koka lexer definition for use in the Python Pygments library.
# Just drop it into "pygments/lexers/functional.py" file and 
#  add "KokaLexer" to the __all__ definition at the top of that file.

class KokaLexer(RegexLexer):
    """
    Lexer for the Koka language.
    """

    name = 'Koka'
    aliases = ['koka']
    filenames = ['*.kk', '*.kki']
    mimetypes = ['text/x-koka']

    keywords = [
        'infix', 'infixr', 'infixl', 'prefix', 'postfix',
        'type', 'cotype', 'rectype', 'alias',
        'struct', 'con',
        'fun', 'function', 'val', 'var',
        'external',
        'if', 'then', 'else', 'elif', 'return', 'match',
        'private', 'public', 'private',
        'module', 'import', 'as',
        'include', 'inline',
        'rec',
        'try', 'yield', 'enum',
        'interface', 'instance'
    ]

    # keywords that are followed by a type
    typeStartKeywords = [
        'type','cotype','rectype','alias','struct','enum'
    ]

    # keywords valid in a type
    typekeywords = [
        'forall', 'exists', 'some', 'with'      
    ]

    # builtin names and special names
    builtin = [
        'for', 'while', 'repeat',
        'foreach', 'foreach-indexed', 
        'error', 'catch', 'finally',
        'cs', 'js', 'file', 'ref', 'assigned'
    ]

    # symbols that can be in an operator
    symbols = '[\$%&\*\+@!/\\\^~=\.:\-\?\|<>]+'

    # symbol boundary: an operator keyword should not be followed by any of these
    sboundary = '(?!'+symbols+')'

    # name boundary: a keyword should not be followed by any of these
    boundary = '(?![a-zA-Z0-9_\\-])'

    # main lexer
    tokens = {
        'root': [       
            include('whitespace'),

            # go into type mode
            (r'::?'+sboundary, Keyword.Type, 'type'),
            (r'alias'+boundary, Keyword,'alias-type'),
            (r'struct'+boundary, Keyword,'struct-type'),
            (r'(%s)' % '|'.join(typeStartKeywords)+boundary, Keyword, 'type'),

            # special sequences of tokens (we use ?: for non-capturing group as required by 'bygroups')
            (r'(module)(\s*)((?:interface)?)(\s*)((?:[a-z](?:[a-zA-Z0-9_]|\-[a-zA-Z])*\.)*[a-z](?:[a-zA-Z0-9_]|\-[a-zA-Z])*)'
                , bygroups(Keyword,Text,Keyword,Text,Name.Namespace)),
            (r'(import)(\s+)((?:[a-z](?:[a-zA-Z0-9_]|\-[a-zA-Z])*\.)*[a-z](?:[a-zA-Z0-9_]|\-[a-zA-Z])*)(\s*)((?:as)?)((?:[A-Z](?:[a-zA-Z0-9_]|\-[a-zA-Z])*)?)'
                , bygroups(Keyword,Text,Name.Namespace,Text,Keyword,Name.Namespace)),

            # keywords
            (r'(%s)' % '|'.join(typekeywords) + boundary, Keyword.Type),
            (r'(%s)' % '|'.join(keywords) + boundary, Keyword),
            (r'(%s)' % '|'.join(builtin) + boundary, Keyword.Pseudo),
            (r'::|:=|\->|[=\.:]' + sboundary, Keyword),
            (r'\-' + sboundary, Generic.Strong),

            # names
            (r'[A-Z]([a-zA-Z0-9_]|\-[a-zA-Z])*(?=\.)',Name.Namespace),            
            (r'[A-Z]([a-zA-Z0-9_]|\-[a-zA-Z])*(?!\.)',Name.Class),
            (r'[a-z]([a-zA-Z0-9_]|\-[a-zA-Z])*',Name),
            (r'_([a-zA-Z0-9_]|\-[a-zA-Z])*',Name.Variable),

            # literal string
            (r'@"', String.Double, 'litstring'),
            
            # operators
            (symbols, Operator),
            (r'`', Operator),
            (r'[\{\}\(\)\[\];,]', Punctuation),

            # literals. No check for literal characters with too many characters in it.
            (r'[0-9]+\.[0-9]+([eE][\-\+]?[0-9]+)?', Number.Float),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),

            (r"'", String.Char, 'char'),
            (r'"', String.Double, 'string'),
        ],

        # type started by alias
        'alias-type': [
            (r'=',Keyword),
            include('type')
        ],

        # type started by struct
        'struct-type': [
            (r'(?=\((?!,*\)))',Punctuation,'#pop'),
            include('type')
        ],

        # type started by colon
        'type': [
            (r'[\(\[<]', Keyword.Type, 'type-nested' ),
            include('type-content')
        ],

        # type nested in brackets: can contain parameters, comma etc.
        'type-nested': [
            (r'[\)\]>]', Keyword.Type, '#pop' ),
            (r'[\(\[<]', Keyword.Type, 'type-nested' ),
            (r'[,]', Keyword.Type),
            (r'([a-z](?:[a-zA-Z0-9_]|\-[a-zA-Z])*)(\s*)(:)(?![:])',bygroups(Name.Variable,Text,Keyword.Type)),  # parameter name            
            include('type-content')            
        ],

        # shared contents of a type
        'type-content': [
            include('whitespace'),

            # keywords
            (r'(%s)' % '|'.join(typekeywords) + boundary, Keyword.Type),
            (r'(?=((%s)' % '|'.join(keywords) + boundary + '))', Keyword, '#pop'),  # need to match because names overlap..

            # kinds
            (r'[EPH]' + boundary, Keyword.Type),
            (r'[\*\!]', Keyword.Type),            

            # type names
            (r'[A-Z]([a-zA-Z0-9_]|\-[a-zA-Z])*(?=\.)',Name.Namespace),            
            (r'[A-Z]([a-zA-Z0-9_]|\-[a-zA-Z])*(?!\.)',Name.Class),
            (r'[a-z][0-9]*(?![a-zA-Z_\-])',Keyword.Type),            # Generic.Emph
            (r'_([a-zA-Z0-9_]|\-[a-zA-Z])*',Keyword.Type),           # Generic.Emph
            (r'[a-z]([a-zA-Z0-9_]|\-[a-zA-Z])*',Keyword.Type),
            
            # type keyword operators
            (r'::|\->|[\.:|]', Keyword.Type),

            #catchall
            (r'', Text, '#pop')
        ],

        # comments and literals
        'whitespace': [
            (r'\s+', Text),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*$', Comment.Single)
        ],
        'comment': [
            (r'[^/\*]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[\*/]', Comment.Multiline),            
        ],
        'litstring': [
            (r'[^"]+', String.Double),
            (r'""', String.Escape),
            (r'"', String.Double, '#pop'),
        ],
        'string': [
            (r'[^\\"\n]+', String.Double),
            include('escape-sequence'),
            (r'["\n]', String.Double, '#pop'),
        ],
        'char': [
            (r'[^\\\'\n]+', String.Char),
            include('escape-sequence'),
            (r'[\'\n]', String.Char, '#pop'),
        ],
        'escape-sequence': [
            (r'\\[abfnrtv0\\\"\'\?]', String.Escape),
            (r'\\x[0-9a-fA-F]{2}', String.Escape),
            (r'\\u[0-9a-fA-F]{4}', String.Escape),
            (r'\\U[0-9a-fA-F]{6}', String.Escape)
        ]        
    }

