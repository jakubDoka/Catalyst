function list($, item, start, sep, end) {
  return seq(start, optional($.new_line), optional(seq(
      item,
      repeat(seq(sep, optional($.new_line), item)),
      optional(sep),
      optional($.new_line),
    )), end)
}

const PATH_PREC = 0

module.exports = grammar({
  name: 'catalyst',
  
  extras: $ => [/[ \t\r]*/, $.comment, $.multi_comment],
  
  rules: {
    source_file: $ => seq(
      optional($.imports),
      repeat(seq(optional($.new_line), $._item)),
      optional($.new_line)
    ),

    imports: $ => seq('use', list($, $.import, '{', $.new_line, '}')),
    
    import: $ => seq($.vis, optional($.name), $.str),
    
    _item: $ => choice(
      $.fn,
    ),

    fn: $ => seq(optional($.vis), $.sig, $._fn_body),

    sig: $ => seq(
      'fn',
      optional($.generics),
      $.name,
      optional($.sig_args),
      optional(seq('->', $.type))
    ),

    generics: $ => list($, $.generic_param, '[', ',', ']'),

    sig_args: $ => list($, $.sig_arg, '(', ',', ')'),

    generic_param: $ => seq($.name, optional(seq(':', $.path, repeat(seq('+', $.path))))),

    sig_arg: $ => seq($.pat, ':', $.type),

    pat: $ => choice(
      seq('mut', $.pat),
      $.name,
      $.struct_pat,
      $.enum_pat,
      $.int,
      $.str,
      $.char,
    ),

    struct_pat: $ => seq('\\', list($, $._struct_pat_field, '{', ',', '}')),

    _struct_pat_field: $ => choice(
      seq(optional('mut'), $.name),
      seq($.name, ':', $.pat),
      '..',
    ),

    enum_pat: $ => seq('\\', field("tag", $.name), optional(seq('~', $.pat))),

    _fn_body: $ => choice(
      seq('=>', $._expr),
      $.block,
      'extern',
    ),

    type: $ => choice(
      $.path,
      $.ptr,
      $.tuple,
    ),

    ptr: $ => seq('^', optional($._mutability), $.type),

    _mutability: $ => choice(
      'mut',
      seq('use', $.path),
    ),

    tuple: $ => list($,$.type, '(', ',', ')'),

    path: $ => prec.right(PATH_PREC, seq(
      field("slash", optional('\\')),
      field("start", $.path_seg),
      field("tail", repeat(seq('\\', $.path_seg))),
    )),

    path_seg: $ => choice($.name, list($, $.type, '[', ',', ']')),

    _expr: $ => choice(
      $.op,
      $._unit_expr,
    ),

    _unit_expr: $ => choice(
      $.return,
      $.int,
      $.bool,
      $.str,
      $.char,
      $.block,
      $.path,
    ),

    block: $ => list($, $._expr, '{', $.new_line, '}'),

    return: $ => seq('return', optional($._expr)),

    vis: $ => choice('pub', 'priv'),

    new_line: $ => prec.right(0, repeat1(choice('\n', ';'))),
   
    label: $ => /'[a-zA-Z0-9_]+/,
    name: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    marcro: $ => /[a-zA-Z_][a-zA-Z0-9_]*!/,
    int: $ => /[0-9]+((u)(32)|uint)?/,
    str: $ => /"(\\"|[^"])*"/,
    bool: $ => /(true|false)/,
    char: $ => /'(.|\\(n|r|t|\\|'))'/,

    op: $ => {
      const table = [
       ['*', '/', '%'],
       ['+', '-'],
       ['<<', '>>'],
       ['<', '>', '<=', '>='],
       ['==', '!='],
       ['&'],
       ['^'],
       ['|'],
       ['&&'],
       ['||'],
       ['=', '+=', '-=', '*=', '/=', '%=', '<<=', '>>=', '&=', '^=', '|='],
      ]

      var i = 3;
      return choice(...table.map(operator => prec.left(i++, seq(
        $._expr, choice(...operator), $._expr,
      ))))
    },

    
    comment: $ => token(/\/[^\n]/),
    multi_comment: $ => token(
      seq('/*', /(\*[^/]|[/*]|[^*]\/)/, '*/')
    ),

  }
})