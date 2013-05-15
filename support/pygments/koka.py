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
    Lexer for the `Koka <http://koka.codeplex.com>`_
    language.

    *New in Pygments 1.6.*
    """

    name = 'Koka'
    aliases = ['koka']
    filenames = ['*.kk', '*.kki']
    mimetypes = ['text/x-koka']

    keywords = [
        'infix', 'infixr', 'infixl',
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
        'interface', 'instance',
    ]

    # keywords that are followed by a type
    typeStartKeywords = [
        'type', 'cotype', 'rectype', 'alias', 'struct', 'enum',
    ]

    # keywords valid in a type
    typekeywords = [
        'forall', 'exists', 'some', 'with',
    ]

    # builtin names and special names
    builtin = [
        'for', 'while', 'repeat',
        'foreach', 'foreach-indexed',
        'error', 'catch', 'finally',
        'cs', 'js', 'file', 'ref', 'assigned',
    ]

    # symbols that can be in an operator
    symbols = '[\$%&\*\+@!/\\\^~=\.:\-\?\|<>]+'

    # symbol boundary: an operator keyword should not be followed by any of these
    sboundary = '(?!'+symbols+')'

    # name boundary: a keyword should not be followed by any of these
    boundary = '(?![\w/])'

    # koka token abstractions
    tokenType = Name.Attribute
    tokenTypeDef = Name.Class
    tokenConstructor = Generic.Emph

    # main lexer
    tokens = {
        'root': [
            include('whitespace'),

            # go into type mode
            (r'::?' + sboundary, tokenType, 'type'),
            (r'(alias)(\s+)([a-z]\w*)?', bygroups(Keyword,Text,tokenTypeDef), 'alias-type'),
            (r'(struct)(\s+)([a-z]\w*)?',  bygroups(Keyword,Text,tokenTypeDef), 'struct-type'),
            ((r'(%s)' % '|'.join(typeStartKeywords)) +
             r'(\s+)([a-z]\w*)?', bygroups(Keyword,Text,tokenTypeDef), 'type'),
            
            # special sequences of tokens (we use ?: for non-capturing group as
            # required by 'bygroups')
            (r'(module)(\s+)(interface\s+)?((?:[a-z]\w*/)*[a-z]\w*)',
             bygroups(Keyword, Text, Keyword, Name.Namespace)),
            (r'(import)(\s+)((?:[a-z]\w*/)*[a-z]\w*)'
              r'(?:(\s*)(=)(\s*)((?:qualified\s*)?)'
              r'((?:[a-z]\w*/)*[a-z]\w*))?',
             bygroups(Keyword, Text, Name.Namespace,
                      Text, Keyword,Text,Keyword,
                      Name.Namespace)),

            (r'(^(?:(?:public|private)\s*)?(?:function|fun|val))(\s+)([a-z]\w*|\((?:' + symbols + r'|/)\))', 
             bygroups(Keyword,Text,Name.Function)),
            (r'(^(?:(?:public|private)\s*)?external)(\s+)(inline\s+)?([a-z]\w*|\((?:' + symbols + r'|/)\))', 
             bygroups(Keyword,Text,Keyword,Name.Function)),

            # keywords
            (r'(%s)' % '|'.join(typekeywords) + boundary, Keyword.Type),
            (r'(%s)' % '|'.join(keywords) + boundary, Keyword),
            (r'(%s)' % '|'.join(builtin) + boundary, Keyword.Pseudo),
            (r'::?|:=|\->|[=\.]' + sboundary, Keyword),
            
            # names
            (r'((?:[a-z]\w*/)*)([A-Z]\w*)', bygroups(Name.Namespace,tokenConstructor)),
            (r'((?:[a-z]\w*/)*)([a-z]\w*)', bygroups(Name.Namespace,Name)),
            (r'((?:[a-z]\w*/)*)(\((?:' + symbols + r'|/)\))', bygroups(Name.Namespace,Name)),
            (r'_\w*', Name.Variable),

            # literal string
            (r'@"', String.Double, 'litstring'),

            # operators
            (symbols + "|/(?![\*/])", Operator),            
            (r'`', Operator),
            (r'[\{\}\(\)\[\];,]', Punctuation),

            # literals. No check for literal characters with len > 1
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
            (r'(?=\((?!,*\)))',Punctuation, '#pop'),
            include('type')
        ],

        # type started by colon
        'type': [
            (r'[\(\[<]', tokenType, 'type-nested'),
            include('type-content')
        ],

        # type nested in brackets: can contain parameters, comma etc.
        'type-nested': [
            (r'[\)\]>]', tokenType, '#pop'),
            (r'[\(\[<]', tokenType, 'type-nested'),
            (r',', tokenType),
            (r'([a-z]\w*)(\s*)(:)(?!:)',
             bygroups(Name,Text,tokenType)),  # parameter name
            include('type-content')
        ],

        # shared contents of a type
        'type-content': [
            include('whitespace'),

            # keywords
            (r'(%s)' % '|'.join(typekeywords) + boundary, Keyword),
            (r'(?=((%s)' % '|'.join(keywords) + boundary + '))',
             Keyword, '#pop'),  # need to match because names overlap...

            # kinds
            (r'[EPHVX]' + boundary, tokenType),
            
            # type names
            (r'[a-z][0-9]*(?![\w/])', tokenType ),
            (r'_\w*', tokenType.Variable),  # Generic.Emph
            (r'((?:[a-z]\w*/)*)([A-Z]\w*)', bygroups(Name.Namespace,tokenType)),
            (r'((?:[a-z]\w*/)*)([a-z]\w+)', bygroups(Name.Namespace,tokenType)),

            # type keyword operators
            (r'::|\->|[\.:|]', tokenType),

            #catchall
            (r'', Text, '#pop')
        ],

        # comments and literals
        'whitespace': [
            (r'\n\s*#.*$', Comment.Preproc),
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
            (r'\\[nrt\\\"\']', String.Escape),
            (r'\\x[0-9a-fA-F]{2}', String.Escape),
            (r'\\u[0-9a-fA-F]{4}', String.Escape),
            # Yes, \U literals are 6 hex digits.
            (r'\\U[0-9a-fA-F]{6}', String.Escape)
        ]
    }

