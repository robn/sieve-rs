use std::ascii::AsciiExt;
use types::*;
use nom::{IResult,Needed,Err,ErrorKind,eof};

macro_rules! char_between_s(
  ($i:expr, $min:expr, $max:expr) => (
    {
      if $i.is_empty() {
        IResult::Incomplete::<&str,char>(Needed::Size(1))
      }
      else {
        let c = $i.chars().next().unwrap();
        if c >= $min && c <= $max {
          IResult::Done(&$i[1..], c)
        }
        else {
          IResult::Error(Err::Position(ErrorKind::OneOf, $i))
        }
      }
    }
  );
);

macro_rules! char_s(
  ($i:expr, $c:expr) => (
    {
      if $i.is_empty() {
        IResult::Incomplete::<&str,char>(Needed::Size(1))
      }
      else {
        if $i.chars().next().unwrap() == $c {
          IResult::Done(&$i[1..], $c)
        } else {
          IResult::Error(Err::Position(ErrorKind::Char, $i))
        }
      }
    }
  );
);

named!(crlf_s<&str,char>,
  chain!(
    char_s!('\r') ~
    complete!(char_s!('\n')),
      || { '\n' }
  )
);


/* ABNF core productions (RFC 4234) */

/*
   alpha          =  %x41-5A / %x61-7A   ; A-Z / a-z
*/
named!(alpha<&str,char>,
  alt!(
    char_between_s!('A', 'Z') |
    char_between_s!('a', 'z')
  )
);

/*
   digit          =  %x30-39 ; 0-9
*/
// XXX refactor
fn is_digit(c: char) -> bool {
  c >= '0' && c <= '9'
}
named!(digit<&str,char>,
  char_between_s!('0', '9')
);

/*
   sp             =  %x20
*/
named!(sp<&str,char>,
  char_s!(' ')
);

/*
   htab           =  %x09 ; horizontal tab
*/
named!(htab<&str,char>,
  char_s!('\t')
);

/*
   dquote         =  %x22 ; " (Double Quote)
*/
named!(dquote<&str,char>,
  char_s!('"')
);


/* Sieve lexical tokens (RFC 5228 8.1) */

/*
   bracket-comment    = "/*" *not-star 1*STAR
                        *(not-star-slash *not-star 1*STAR) "/"
                          ; No */ allowed inside a comment.
                          ; (No * is allowed unless it is the last
                          ; character, or unless it is followed by a
                          ; character that isn't a slash.)
*/
named!(bracket_comment<&str,&str>,
  chain!(
    tag_s!("/*") ~
    many0!(not_star) ~
    many1!(star) ~
    many0!(
      chain!(
        not_star_slash ~
        many0!(not_star) ~
        many1!(char_s!('*')),
          || {}
      )
    ) ~
    char_s!('/'),
      || { "" }
  )
);

/*
   comment            = bracket-comment / hash-comment
*/
named!(comment<&str,&str>,
  alt!(
    bracket_comment |
    hash_comment
  )
);

/*
   hash-comment       = "#" *octet-not-crlf CRLF
*/
named!(hash_comment<&str,&str>,
  chain!(
    char_s!('#') ~
    many0!(octet_not_crlf) ~
    crlf_s,
      || { "" }
  )
);

/*
   identifier         = (alpha / "_") *(alpha / digit / "_")
*/
named!(identifier<&str,String>,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    h: alt!(
      alpha |
      char_s!('_')
    ) ~
    t: many0!(
      alt!(
        alpha |
        digit |
        char_s!('_')
      )
    ),
      || {
        let mut sh: String = h.to_string();
        let st: String = t.into_iter().collect();
        sh.push_str(&*st);
        sh
      }
  )
);

/*
   multi-line         = "text:" *(sp / htab) (hash-comment / CRLF)
                        *(multiline-literal / multiline-dotstart)
                        "." CRLF
*/
named!(multi_line<&str,String>,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    complete!(tag_s!("text:")) ~
    many0!(
      alt!(sp | htab)
    ) ~
    alt!(
      hash_comment |
      tag_s!("\r\n")
    ) ~
    t: many0!(
      alt!(
        multiline_literal |
        multiline_dotstart
      )
    ) ~
    tag_s!(".\r\n"),
      || { t.join("\n") }
  )
);

/*
   multiline-literal  = [ octet-not-period *octet-not-crlf ] CRLF
*/
named!(multiline_literal<&str,String>,
  chain!(
    l: opt!(
      chain!(
        h: octet_not_period ~
        t: many0!(octet_not_crlf),
          || {
              let mut sh: String = h.to_string();
              let st: String = t.into_iter().collect();
              sh.push_str(&*st);
              sh
          }
      )
    ) ~
    complete!(crlf_s),
      || {
        match l {
          Some(ref s) => s.clone(),
          _           => String::new(),
        }
      }
  )
);

/*
   multiline-dotstart = "." 1*octet-not-crlf CRLF
                          ; A line containing only "." ends the
                          ; multi-line.  Remove a leading '.' if
                          ; followed by another '.'.
*/
named!(multiline_dotstart<&str,String>,
  chain!(
    tag_s!(".") ~
    t: many1!(octet_not_crlf) ~
    complete!(crlf_s),
      || { t.into_iter().collect::<String>() }
  )
);

/*
   not-star           = CRLF / %x01-09 / %x0B-0C / %x0E-29 / %x2B-FF
                          ; either a CRLF pair, OR a single octet
                          ; other than NUL, CR, LF, or star
*/
named!(not_star<&str,char>,
  alt!(
    crlf_s |
    char_between_s!('\u{01}', '\u{09}') |
    char_between_s!('\u{0b}', '\u{0c}') |
    char_between_s!('\u{0e}', '\u{29}') |
    char_between_s!('\u{2b}', '\u{7f}')
  )
);

/*
   not-star-slash     = CRLF / %x01-09 / %x0B-0C / %x0E-29 / %x2B-2E /
                        %x30-FF
                          ; either a CRLF pair, OR a single octet
                          ; other than NUL, CR, LF, star, or slash
*/
named!(not_star_slash<&str,char>,
  alt!(
    crlf_s |
    char_between_s!('\u{01}', '\u{09}') |
    char_between_s!('\u{0b}', '\u{0c}') |
    char_between_s!('\u{0e}', '\u{29}') |
    char_between_s!('\u{2b}', '\u{2e}') |
    char_between_s!('\u{30}', '\u{7f}')
  )
);

/*
   number             = 1*digit [ QUANTIFIER ]
*/
named!(number<&str,usize>,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    d: take_while1_s!(is_digit) ~
    q: opt!(complete!(quantifier)),
      || {
        d.parse::<usize>().unwrap() // XXX errors even possible? too long sequence?
        * match q {
          Some(Quantifier::K) => 2usize.pow(10),
          Some(Quantifier::M) => 2usize.pow(20),
          Some(Quantifier::G) => 2usize.pow(30),
          _                   => 1,
        }
      }
  )
);

/*
   octet-not-crlf     = %x01-09 / %x0B-0C / %x0E-FF
                          ; a single octet other than NUL, CR, or LF
*/
named!(octet_not_crlf<&str,char>,
  alt!(
    char_between_s!('\u{01}', '\u{09}') |
    char_between_s!('\u{0b}', '\u{0c}') |
    char_between_s!('\u{0e}', '\u{ff}')
  )
);

/*
   octet-not-period   = %x01-09 / %x0B-0C / %x0E-2D / %x2F-FF
                          ; a single octet other than NUL,
                          ; CR, LF, or period
*/
named!(octet_not_period<&str,char>,
  alt!(
    char_between_s!('\u{01}', '\u{09}') |
    char_between_s!('\u{0b}', '\u{0c}') |
    char_between_s!('\u{0e}', '\u{2d}') |
    char_between_s!('\u{2f}', '\u{ff}')
  )
);

/*
   octet-not-qspecial = %x01-09 / %x0B-0C / %x0E-21 / %x23-5B / %x5D-FF
                          ; a single octet other than NUL,
                          ; CR, LF, double-quote, or backslash
*/
named!(octet_not_qspecial<&str,char>,
  alt!(
    char_between_s!('\u{01}', '\u{09}') |
    char_between_s!('\u{0b}', '\u{0c}') |
    char_between_s!('\u{0e}', '\u{21}') |
    char_between_s!('\u{23}', '\u{5b}') |
    char_between_s!('\u{5d}', '\u{ff}')
  )
);

/*
   QUANTIFIER         = "K" / "M" / "G"
*/
#[derive(Clone,PartialEq,Debug)]
enum Quantifier {
  K,
  M,
  G,
}
named!(quantifier<&str,Quantifier>,
  chain!(
    q: alt!(
      char_s!('K') |
      char_s!('M') |
      char_s!('G')
    ),
    || {
      match q {
        'K' => Quantifier::K,
        'M' => Quantifier::M,
        'G' => Quantifier::G,
        _   => unreachable!(),
      }
    }
  )
);

/*
   quoted-other       = "\" octet-not-qspecial
                          ; represents just the octet-no-qspecial
                          ; character.  SHOULD NOT be used
*/
named!(quoted_other<&str,char>,
  chain!(
    char_s!('\\') ~
    c: octet_not_qspecial,
      || { c }
  )
);

/*
   quoted-safe        = CRLF / octet-not-qspecial
                          ; either a CRLF pair, OR a single octet other
                          ; than NUL, CR, LF, double-quote, or backslash
*/
named!(quoted_safe<&str,char>,
  alt!(
    complete!(crlf_s) |
    octet_not_qspecial
  )
);

/*
   quoted-special     = "\" (DQUOTE / "\")
                          ; represents just a double-quote or backslash
*/
named!(quoted_special<&str,char>,
  chain!(
    char_s!('\\') ~
    c: complete!(alt!(
      dquote |
      char_s!('\\')
    )),
      || { c }
  )
);

/*
   quoted-string      = DQUOTE quoted-text DQUOTE
*/
named!(quoted_string<&str,String>,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    dquote ~
    s: quoted_text ~
    dquote,
      || { s }
  )
);

/*
   quoted-text        = *(quoted-safe / quoted-special / quoted-other)
*/
named!(quoted_text<&str,String>,
  fold_many0!(
    alt!(
      quoted_safe |
      quoted_special |
      quoted_other
    ),
    String::new(),
    |mut acc: String, c| {
      acc.push(c);
      acc
    }
  )
);

/*
   star               = "*"
*/
named!(star<&str,char>,
  char_s!('*')
);

/*
   tag                = ":" identifier
*/
named!(tag<&str,String>,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    char_s!(':') ~
    t: identifier,
      || { t }
  )
);

/*
   white-space        = 1*(sp / CRLF / htab) / comment
*/
named!(white_space<&str,char>,
  alt!(
    fold_many1!(
      alt_complete!(sp | crlf_s | htab),
    ' ', |_,_| ' ') |
    complete!(comment)
      => { |_| ' ' }
  )
);


/* Sieve grammar (RFC 5228 8.2) */

/*
   argument     = string-list / number / tag
*/
named!(argument<&str,Argument>,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    a: alt!(
      string_list => { |x| Argument::StringList(x) } |
      number      => { |x| Argument::Number(x) } |
      tag         => { |x| Argument::Tag(x) }
    ),
      || { a }
  )
);

/*
   arguments    = *argument [ test / test-list ]
*/
named!(arguments<&str,Arguments>,
  chain!(
    a: many0!(argument) ~
    t: opt!(
      alt!(
        complete!(test_list) |
        complete!(test) => { |t| vec!(t) }
      )
    ),
      || {
        Arguments {
          arguments: a,
          tests: match t {
            Some(ref t) => t.clone(),
            None        => vec!(),
          },
        }
      }
  )
);

/*
   block        = "{" commands "}"
*/
named!(block<&str,Vec<Command> >,
  chain!(
    many0!(white_space) ~ // consume whitespace in front of lexical token
    char_s!('{') ~
    c: commands ~
    many0!(white_space) ~
    char_s!('}') ~
    many0!(white_space),
      || { c }
  )
);

/*
   command      = identifier arguments (";" / block)
*/
named!(command<&str,Command>,
  chain!(
    id: identifier ~
    a: arguments ~
    c: alt!(
      char_s!(';') => { |_| vec!() } |
      block
    ),
      || {
        match &*id.to_ascii_lowercase() {
          // Control commands (RFC 5228 s3)
          "if"      => Command::If(a.tests.clone(), c),
          "elsif"   => Command::ElsIf(a.tests.clone(), c),
          "else"    => Command::Else(c),
          "require" => Command::Require(a.arguments.clone()),
          "stop"    => Command::Stop,

          // Action commands (RFC 5228 s4)
          "fileinto" => Command::FileInto(a.arguments.clone()),
          "redirect" => Command::Redirect(a.arguments.clone()),
          "keep"     => Command::Keep,
          "discard"  => Command::Discard,

          _ => Command::Unknown(id),
        }
      }
  )
);

/*
   commands     = *command
*/
named!(commands<&str,Vec<Command> >,
  many0!(command)
);

/*
   start        = commands
*/ 
named!(pub start<&str,Vec<Command> >,
  chain!(
    c: call!(commands) ~
    eof,
      || { c }
  )
);

/*
   string       = quoted-string / multi-line
*/
named!(string<&str,String>,
  alt!(quoted_string | multi_line)
);

/*
   string-list  = "[" string *("," string) "]" / string
                    ; if there is only a single string, the brackets
                    ; are optional
*/
named!(string_list<&str,Vec<String> >,
  alt!(
    chain!(
      char_s!('[') ~
      h: string ~
      t: many0!(
        chain!(
          char_s!(',') ~
          s: string,
            || { s }
        )
      ) ~
      char_s!(']'),
        || {
          let mut v: Vec<String> = t.into_iter().collect();
          v.insert(0, h);
          v
        }
    ) |
    string => { |s| vec!(s) }
  )
);

/*
   test         = identifier arguments
*/
named!(test<&str,Test>,
  chain!(
    id: identifier ~
    a: arguments,
      || Test {
        identifier: id,
        arguments: a,
      }
  )
);

/*
   test-list    = "(" test *("," test) ")"
*/
named!(test_list<&str,Vec<Test> >,
  chain!(
    many0!(white_space) ~
    char_s!('(') ~
    h: test ~
    t: many0!(
      chain!(
        char_s!(',') ~
        t: test,
          || { t }
      )
    ) ~
    char_s!(')'),
      || {
        let mut v: Vec<Test> = t.into_iter().collect();
        v.insert(0, h);
        v
      }
  )
);


/* Sieve statement elements (RFC 5228 8.3) */

/*
   ADDRESS-PART = ":localpart" / ":domain" / ":all"

   COMPARATOR   = ":comparator" string

   MATCH-TYPE   = ":is" / ":contains" / ":matches"
*/


#[cfg(test)]
mod tests {
  use super::{alpha,digit,sp,htab};
  use super::{bracket_comment,comment,hash_comment};
  use super::{identifier};
  use super::{multiline_literal,multiline_dotstart,multi_line};
  use super::{not_star,not_star_slash};
  use super::{number};
  use super::{octet_not_crlf,octet_not_period,octet_not_qspecial};
  use super::{quantifier,Quantifier};
  use super::{quoted_other,quoted_safe,quoted_special};
  use super::{quoted_string,quoted_text};
  use super::{tag,white_space};
  use super::{string,string_list};
  use super::{argument,arguments};
  use super::{test,test_list};
  use super::start;
  use types::*;
  use nom::IResult::*;
  use nom::Err::*;
  use nom::ErrorKind::*;

  macro_rules! vecstring {
    ($($s:expr),*) => {
      vec!($($s,)*).iter().map(|s| s.to_string()).collect()
    }
  }

  #[test]
  fn alpha_test() {
    assert_eq!(alpha("a"),  Done("",  'a'));
    assert_eq!(alpha("z"),  Done("",  'z'));
    assert_eq!(alpha("AZ"), Done("Z", 'A'));
    assert_eq!(alpha("2"),  Error(Position(Alt, "2")));
  }

  #[test]
  fn digit_test() {
    assert_eq!(digit("0"),  Done("",  '0'));
    assert_eq!(digit("9"),  Done("",  '9'));
    assert_eq!(digit("09"), Done("9", '0'));
    assert_eq!(digit("a"),  Error(Position(OneOf, "a")));
  }

  #[test]
  fn sp_test() {
    assert_eq!(sp(" "),  Done("",   ' '));
    assert_eq!(sp(" z"), Done("z",  ' '));
    assert_eq!(sp("a"),  Error(Position(Char, "a")));
  }

  #[test]
  fn htab_test() {
    assert_eq!(htab("\t"),  Done("",   '\t'));
    assert_eq!(htab("\tz"), Done("z",  '\t'));
    assert_eq!(htab("a"),   Error(Position(Char, "a")));
  }

  #[test]
  fn bracket_comment_test() {
    assert_eq!(bracket_comment("/* a comment */"), Done("", ""));
  }

  #[test]
  fn comment_test() {
    assert_eq!(comment("/* a comment */"), Done("", ""));
    assert_eq!(comment("# a comment\r\n"), Done("", ""));
  }

  #[test]
  fn hash_comment_test() {
    assert_eq!(hash_comment("# a comment\r\n"), Done("", ""));
  }

  #[test]
  fn identifier_test() {
    assert_eq!(identifier("a"),   Done("", "a".to_string()));
    assert_eq!(identifier("_"),   Done("", "_".to_string()));
    assert_eq!(identifier("abc"), Done("", "abc".to_string()));
    assert_eq!(identifier("_bc"), Done("", "_bc".to_string()));
    assert_eq!(identifier("_b_"), Done("", "_b_".to_string()));
    assert_eq!(identifier("___"), Done("", "___".to_string()));
    assert_eq!(identifier("ab1"), Done("", "ab1".to_string()));

    assert_eq!(identifier("1"),   Error(Position(Alt, "1")));
    assert_eq!(identifier("1ab"), Error(Position(Alt, "1ab")));
  }

  #[test]
  fn mutiline_literal_test() {
    assert_eq!(multiline_literal("abcdef\r\n"), Done("", "abcdef".to_string()));
    assert_eq!(multiline_literal("abc.ef\r\n"), Done("", "abc.ef".to_string()));

    assert_eq!(multiline_literal(".bcdef\r\n"), Error(Position(Char, ".bcdef\r\n")));
    assert_eq!(multiline_literal("abcdef"),     Error(Position(Complete, "")));
  }

  #[test]
  fn multiline_dotstart_test() {
    assert_eq!(multiline_dotstart(".abcdef\r\n"), Done("", "abcdef".to_string()));

    assert_eq!(multiline_dotstart("abcdef\r\n"),    Error(Position(TagStr, "abcdef\r\n")));
    assert_eq!(multiline_dotstart(".abcdef"),       Error(Position(Complete, "")));
    assert_eq!(multiline_dotstart(".abc\ndef\r\n"), Error(Position(Char, "\ndef\r\n")));
  }

  #[test]
  fn multi_line_test() {
    assert_eq!(multi_line("text:\r\nfoo\r\n.\r\n"),        Done("", "foo".to_string()));
    assert_eq!(multi_line("text:\r\n.foo\r\n.\r\n"),       Done("", "foo".to_string()));
    assert_eq!(multi_line("text:\r\nfoo\r\nbar\r\n.\r\n"), Done("", "foo\nbar".to_string()));
  }

  #[test]
  fn not_star_test() {
    assert_eq!(not_star("a"),    Done("", 'a'));
    assert_eq!(not_star("1"),    Done("", '1'));
    assert_eq!(not_star("%"),    Done("", '%'));
    assert_eq!(not_star("/"),    Done("", '/'));
    assert_eq!(not_star("*"),    Error(Position(Alt, "*")));
    assert_eq!(not_star("\""),   Done("", '"'));
    assert_eq!(not_star("\\"),   Done("", '\\'));
    assert_eq!(not_star("."),    Done("", '.'));
    assert_eq!(not_star("\r\n"), Done("", '\n'));
    assert_eq!(not_star("\r"),   Error(Position(Alt, "\r")));
    assert_eq!(not_star("\n"),   Error(Position(Alt, "\n")));
    assert_eq!(not_star("\0"),   Error(Position(Alt, "\0")));
  }

  #[test]
  fn not_star_slash_test() {
    assert_eq!(not_star_slash("a"),    Done("", 'a'));
    assert_eq!(not_star_slash("1"),    Done("", '1'));
    assert_eq!(not_star_slash("%"),    Done("", '%'));
    assert_eq!(not_star_slash("/"),    Error(Position(Alt, "/")));
    assert_eq!(not_star_slash("*"),    Error(Position(Alt, "*")));
    assert_eq!(not_star_slash("\""),   Done("", '"'));
    assert_eq!(not_star_slash("\\"),   Done("", '\\'));
    assert_eq!(not_star_slash("."),    Done("", '.'));
    assert_eq!(not_star_slash("\r\n"), Done("", '\n'));
    assert_eq!(not_star_slash("\r"),   Error(Position(Alt, "\r")));
    assert_eq!(not_star_slash("\n"),   Error(Position(Alt, "\n")));
    assert_eq!(not_star_slash("\0"),   Error(Position(Alt, "\0")));
  }

  #[test]
  fn number_test() {
    assert_eq!(number("1"),          Done("", 1));
    assert_eq!(number("12"),         Done("", 12));
    assert_eq!(number("123"),        Done("", 123));
    assert_eq!(number("1234"),       Done("", 1234));
    assert_eq!(number("12345"),      Done("", 12345));
    assert_eq!(number("123456"),     Done("", 123456));
    assert_eq!(number("1234567"),    Done("", 1234567));
    assert_eq!(number("12345678"),   Done("", 12345678));
    assert_eq!(number("123456789"),  Done("", 123456789));
    assert_eq!(number("1234567890"), Done("", 1234567890));

    assert_eq!(number("0"),    Done("", 0));
    assert_eq!(number("00"),   Done("", 0));
    assert_eq!(number("000"),  Done("", 0));
    assert_eq!(number("01"),   Done("", 1));
    assert_eq!(number("012"),  Done("", 12));
    assert_eq!(number("0102"), Done("", 102));

    assert_eq!(number("1K"), Done("", 1*2usize.pow(10)));
    assert_eq!(number("1M"), Done("", 1*2usize.pow(20)));
    assert_eq!(number("1G"), Done("", 1*2usize.pow(30)));

    assert_eq!(number("123K"), Done("", 123*2usize.pow(10)));
    assert_eq!(number("123M"), Done("", 123*2usize.pow(20)));
    assert_eq!(number("123G"), Done("", 123*2usize.pow(30)));
  }

  #[test]
  fn quantifier_test() {
    assert_eq!(quantifier("K"), Done("", Quantifier::K));
    assert_eq!(quantifier("M"), Done("", Quantifier::M));
    assert_eq!(quantifier("G"), Done("", Quantifier::G));
    assert_eq!(quantifier("T"), Error(Position(Alt, "T")));
  }

  #[test]
  fn octet_not_crlf_test() {
    assert_eq!(octet_not_crlf("a"),    Done("", 'a'));
    assert_eq!(octet_not_crlf("1"),    Done("", '1'));
    assert_eq!(octet_not_crlf("%"),    Done("", '%'));
    assert_eq!(octet_not_crlf("/"),    Done("", '/'));
    assert_eq!(octet_not_crlf("*"),    Done("", '*'));
    assert_eq!(octet_not_crlf("\""),   Done("", '"'));
    assert_eq!(octet_not_crlf("\\"),   Done("", '\\'));
    assert_eq!(octet_not_crlf("."),    Done("", '.'));
    assert_eq!(octet_not_crlf("\r\n"), Error(Position(Alt, "\r\n")));
    assert_eq!(octet_not_crlf("\r"),   Error(Position(Alt, "\r")));
    assert_eq!(octet_not_crlf("\n"),   Error(Position(Alt, "\n")));
    assert_eq!(octet_not_crlf("\0"),   Error(Position(Alt, "\0")));
  }

  #[test]
  fn octet_not_period_test() {
    assert_eq!(octet_not_period("a"),    Done("", 'a'));
    assert_eq!(octet_not_period("1"),    Done("", '1'));
    assert_eq!(octet_not_period("%"),    Done("", '%'));
    assert_eq!(octet_not_period("/"),    Done("", '/'));
    assert_eq!(octet_not_period("*"),    Done("", '*'));
    assert_eq!(octet_not_period("\""),   Done("", '"'));
    assert_eq!(octet_not_period("\\"),   Done("", '\\'));
    assert_eq!(octet_not_period("."),    Error(Position(Alt, ".")));
    assert_eq!(octet_not_period("\r\n"), Error(Position(Alt, "\r\n")));
    assert_eq!(octet_not_period("\r"),   Error(Position(Alt, "\r")));
    assert_eq!(octet_not_period("\n"),   Error(Position(Alt, "\n")));
    assert_eq!(octet_not_period("\0"),   Error(Position(Alt, "\0")));
  }

  #[test]
  fn octet_not_qspecial_test() {
    assert_eq!(octet_not_qspecial("a"),    Done("", 'a'));
    assert_eq!(octet_not_qspecial("1"),    Done("", '1'));
    assert_eq!(octet_not_qspecial("%"),    Done("", '%'));
    assert_eq!(octet_not_qspecial("/"),    Done("", '/'));
    assert_eq!(octet_not_qspecial("*"),    Done("", '*'));
    assert_eq!(octet_not_qspecial("."),    Done("", '.'));
    assert_eq!(octet_not_qspecial("\""),   Error(Position(Alt, "\"")));
    assert_eq!(octet_not_qspecial("\\"),   Error(Position(Alt, "\\")));
    assert_eq!(octet_not_qspecial("\r\n"), Error(Position(Alt, "\r\n")));
    assert_eq!(octet_not_qspecial("\r"),   Error(Position(Alt, "\r")));
    assert_eq!(octet_not_qspecial("\n"),   Error(Position(Alt, "\n")));
    assert_eq!(octet_not_qspecial("\0"),   Error(Position(Alt, "\0")));
  }

  #[test]
  fn quoted_other_test() {
    assert_eq!(quoted_other("\\a"),  Done("", 'a'));
    assert_eq!(quoted_other("\\1"),  Done("", '1'));
    assert_eq!(quoted_other("\\."),  Done("", '.'));
    assert_eq!(quoted_other("\\\\"), Error(Position(Alt, "\\")));
    assert_eq!(quoted_other("\\\""), Error(Position(Alt, "\"")));
  }

  #[test]
  fn quoted_safe_test() {
    assert_eq!(quoted_safe("a"),    Done("", 'a'));
    assert_eq!(quoted_safe("1"),    Done("", '1'));
    assert_eq!(quoted_safe("%"),    Done("", '%'));
    assert_eq!(quoted_safe("/"),    Done("", '/'));
    assert_eq!(quoted_safe("*"),    Done("", '*'));
    assert_eq!(quoted_safe("."),    Done("", '.'));
    assert_eq!(quoted_safe("\""),   Error(Position(Alt, "\"")));
    assert_eq!(quoted_safe("\\"),   Error(Position(Alt, "\\")));
    assert_eq!(quoted_safe("\r\n"), Done("", '\n'));
    assert_eq!(quoted_safe("\r"),   Error(Position(Alt, "\r")));
    assert_eq!(quoted_safe("\n"),   Error(Position(Alt, "\n")));
    assert_eq!(quoted_safe("\0"),   Error(Position(Alt, "\0")));
  }

  #[test]
  fn quoted_special_test() {
    assert_eq!(quoted_special("\\\""), Done("", '\"'));
    assert_eq!(quoted_special("\\\\"), Done("", '\\'));
    assert_eq!(quoted_special("\\"),   Error(Position(Complete, "")));
    assert_eq!(quoted_special("\\1"),  Error(Position(Alt, "1")));
  }

  #[test]
  fn quotes_string_test() {
    assert_eq!(quoted_string("\"abc123\""),     Done("", "abc123".to_string()));
    assert_eq!(quoted_string("\"abc\r\n123\""), Done("", "abc\n123".to_string()));
  }

  #[test]
  fn quoted_text_test() {
    assert_eq!(quoted_text("abc123"), Done("", "abc123".to_string()));
  }

  #[test]
  fn tag_test() {
    assert_eq!(tag(":foo"),  Done("", "foo".to_string()));
    assert_eq!(tag(":_foo"), Done("", "_foo".to_string()));
  }

  #[test]
  fn white_space_test() {
    assert_eq!(white_space("   "),             Done("", ' '));
    assert_eq!(white_space(" \r\n "),          Done("", ' '));
    assert_eq!(white_space(" \u{09} "),        Done("", ' '));
    assert_eq!(white_space("/* beep boop */"), Done("", ' '));
  }

  #[test]
  fn string_test() {
    assert_eq!(string("\"foo\""),               Done("", "foo".to_string()));
    assert_eq!(string("text:\r\nfoo\r\n.\r\n"), Done("", "foo".to_string()));
  }

  #[test]
  fn string_list_test() {
    assert_eq!(string_list("\"foo\""),                         Done("", vecstring!("foo")));
    assert_eq!(string_list("[\"foo\"]"),                       Done("", vecstring!("foo")));
    assert_eq!(string_list("[\"foo\",\"bar\"]"),               Done("", vecstring!("foo","bar")));
    assert_eq!(string_list("[text:\r\nfoo\r\n.\r\n,\"bar\"]"), Done("", vecstring!("foo","bar")));
  }

  #[test]
  fn argument_test() {
    assert_eq!(argument("\"foo\""), Done("", Argument::StringList(vecstring!("foo"))));
    assert_eq!(argument("123"),     Done("", Argument::Number(123)));
    assert_eq!(argument(":foo"),    Done("", Argument::Tag("foo".to_string())));
  }

  #[test]
  fn arguments_test() {
    assert_eq!(arguments("\"foo\""), Done("",
      Arguments {
        arguments: vec!(Argument::StringList(vecstring!("foo"))),
        tests: vec!()
      }
    ));
  }

  #[test]
  fn test_test() {
    assert_eq!(test("a \"a\""), Done("",
      Test {
        identifier: "a".to_string(),
        arguments: Arguments {
          arguments: vec!(Argument::StringList(vecstring!("a"))),
          tests: vec!(),
        },
      }
    ));
  }

  #[test]
  fn test_list_test() {
    assert_eq!(test_list("(a \"a\")"), Done("", vec!(
      Test {
        identifier: "a".to_string(),
        arguments: Arguments {
          arguments: vec!(Argument::StringList(vecstring!("a"))),
          tests: vec!(),
        },
      },
    )));

    assert_eq!(test_list(" (NOT address :all :contains\r\n[\"To\", \"Cc\", \"Bcc\"] \"me@example.com\",\r\n header :matches \"subject\"\r\n [\"*make*money*fast*\", \"*university*dipl*mas*\"])"),
      Done("", vec!(
        Test {
          identifier: "NOT".to_string(),
          arguments: Arguments {
            arguments: vec!(),
            tests: vec!(
              Test {
                identifier: "address".to_string(),
                arguments: Arguments {
                  arguments: vec!(
                    Argument::Tag("all".to_string()),
                    Argument::Tag("contains".to_string()),
                    Argument::StringList(vecstring!("To", "Cc", "Bcc")),
                    Argument::StringList(vecstring!("me@example.com"))
                  ),
                  tests: vec!()
                }
              }
            )
          }
        },
        Test {
          identifier: "header".to_string(),
          arguments: Arguments {
            arguments: vec!(
              Argument::Tag("matches".to_string()),
              Argument::StringList(vecstring!("subject")),
              Argument::StringList(vecstring!("*make*money*fast*", "*university*dipl*mas*")),
            ),
            tests: vec!()
          }
        }
      ))
    );
  }

  #[test]
  fn start_test() {
    let src: &'static str = include_str!("../testdata/rfc5228-full.sieve");
    assert_eq!(start(src),
      Done("", vec!(
        Command::Require(vec!(
          Argument::StringList(vecstring!("fileinto")),
        )),
        Command::If(
          vec!(
            Test {
              identifier: "header".to_string(),
              arguments: Arguments {
                arguments: vec!(
                  Argument::Tag("is".to_string()),
                  Argument::StringList(vecstring!("Sender")),
                  Argument::StringList(vecstring!("owner-ietf-mta-filters@imc.org")),
                ),
                tests: vec!(),
              }
            }
          ), vec!(
            Command::FileInto(vec!(
              Argument::StringList(vecstring!("filter"))
            )),
          ),
        ),
        Command::ElsIf(
          vec!(
            Test {
              identifier: "address".to_string(),
              arguments: Arguments {
                arguments: vec!(
                  Argument::Tag("DOMAIN".to_string()),
                  Argument::Tag("is".to_string()),
                  Argument::StringList(vecstring!("From", "To")),
                  Argument::StringList(vecstring!("example.com"))
                ),
                tests: vec!()
              }
            }
          ), vec!(
            Command::Keep,
          ),
        ),
        Command::ElsIf(
          vec!(
            Test {
              identifier: "anyof".to_string(),
              arguments: Arguments {
                arguments: vec!(),
                tests: vec!(
                  Test {
                    identifier: "NOT".to_string(),
                    arguments: Arguments {
                      arguments: vec!(),
                      tests: vec!(
                        Test {
                          identifier: "address".to_string(),
                          arguments: Arguments {
                            arguments: vec!(
                              Argument::Tag("all".to_string()),
                              Argument::Tag("contains".to_string()),
                              Argument::StringList(vecstring!("To", "Cc", "Bcc")),
                              Argument::StringList(vecstring!("me@example.com"))
                            ),
                            tests: vec!(),
                          }
                        }
                      )
                    }
                  },
                  Test {
                    identifier: "header".to_string(),
                    arguments: Arguments {
                      arguments: vec!(
                        Argument::Tag("matches".to_string()),
                        Argument::StringList(vecstring!("subject")),
                        Argument::StringList(vecstring!("*make*money*fast*", "*university*dipl*mas*"))
                      ),
                      tests: vec!(),
                    }
                  }
                )
              }
            }
          ), vec!(
            Command::FileInto(vec!(
              Argument::StringList(vecstring!("spam"))
            )),
          ),
        ),
        Command::Else(vec!(
          Command::FileInto(vec!(
            Argument::StringList(vecstring!("personal"))
          )),
        )),
      ))
    )
  }
}
