(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;
var $author$project$CoreTypes$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $author$project$CoreTypes$UrlRequested = function (a) {
	return {$: 'UrlRequested', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Letter$Init = {$: 'Init'};
var $author$project$Letter$NotSent = {$: 'NotSent'};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$CoreTypes$GotReadLetterResp = function (a) {
	return {$: 'GotReadLetterResp', a: a};
};
var $elm$url$Url$Builder$toQueryPair = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return key + ('=' + value);
};
var $elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			$elm$core$String$join,
			'&',
			A2($elm$core$List$map, $elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var $elm$url$Url$Builder$absolute = F2(
	function (pathSegments, parameters) {
		return '/' + (A2($elm$core$String$join, '/', pathSegments) + $elm$url$Url$Builder$toQuery(parameters));
	});
var $author$project$Common$Urls$mkBackendUrl = F2(
	function (pathSegments, queryParams) {
		return A2(
			$elm$url$Url$Builder$absolute,
			A2($elm$core$List$cons, 'api', pathSegments),
			queryParams);
	});
var $author$project$Common$Urls$backendReadLetterUrl = A2(
	$author$project$Common$Urls$mkBackendUrl,
	_List_fromArray(
		['read-letter']),
	_List_Nil);
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{body: $elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$Letter$LetterMeta = F2(
	function (letter, readCount) {
		return {letter: letter, readCount: readCount};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Letter$Letter = F2(
	function (body, maxReadCount) {
		return {body: body, maxReadCount: maxReadCount};
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Letter$letterJsonDec = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Letter$Letter,
	A2($elm$json$Json$Decode$field, 'body', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'maxReadCount', $elm$json$Json$Decode$int));
var $author$project$Letter$letterMetaJsonDec = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Letter$LetterMeta,
	A2($elm$json$Json$Decode$field, 'letter', $author$project$Letter$letterJsonDec),
	A2($elm$json$Json$Decode$field, 'readCount', $elm$json$Json$Decode$int));
var $author$project$Main$getLetterReq = function (letterId) {
	return $elm$http$Http$get(
		{
			expect: A2($elm$http$Http$expectJson, $author$project$CoreTypes$GotReadLetterResp, $author$project$Letter$letterMetaJsonDec),
			url: $author$project$Common$Urls$backendReadLetterUrl + ('/' + letterId)
		});
};
var $author$project$Route$NotFound = {$: 'NotFound'};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Route$About = {$: 'About'};
var $author$project$Route$Chat = function (a) {
	return {$: 'Chat', a: a};
};
var $author$project$Route$ConfigChat = {$: 'ConfigChat'};
var $author$project$Route$ReadLetter = function (a) {
	return {$: 'ReadLetter', a: a};
};
var $author$project$Route$Root = {$: 'Root'};
var $author$project$Route$WriteLetter = {$: 'WriteLetter'};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return $elm$url$Url$Parser$Parser(
			function (_v0) {
				var visited = _v0.visited;
				var unvisited = _v0.unvisited;
				var params = _v0.params;
				var frag = _v0.frag;
				var value = _v0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _v2 = stringToSomething(next);
					if (_v2.$ === 'Just') {
						var nextValue = _v2.a;
						return _List_fromArray(
							[
								A5(
								$elm$url$Url$Parser$State,
								A2($elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var $elm$url$Url$Parser$string = A2($elm$url$Url$Parser$custom, 'STRING', $elm$core$Maybe$Just);
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $author$project$Route$routeParser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2($elm$url$Url$Parser$map, $author$project$Route$Root, $elm$url$Url$Parser$top),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$About,
			$elm$url$Url$Parser$s('about')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$ReadLetter,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('read-letter'),
				$elm$url$Url$Parser$string)),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$WriteLetter,
			$elm$url$Url$Parser$s('write-letter')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Chat,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('chat'),
				$elm$url$Url$Parser$string)),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$ConfigChat,
			$elm$url$Url$Parser$s('config-chat'))
		]));
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Route$getRoute = function (url) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Route$NotFound,
		A2($elm$url$Url$Parser$parse, $author$project$Route$routeParser, url));
};
var $author$project$CoreTypes$GotViewport = function (a) {
	return {$: 'GotViewport', a: a};
};
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $author$project$Main$getViewportCmd = A2($elm$core$Task$perform, $author$project$CoreTypes$GotViewport, $elm$browser$Browser$Dom$getViewport);
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$port_InitWs = _Platform_outgoingPort('port_InitWs', $elm$json$Json$Encode$string);
var $author$project$UserStatus$Other = {$: 'Other'};
var $author$project$Main$routeToInitUserStatus = function (route) {
	return $author$project$UserStatus$Other;
};
var $joneshf$elm_tagged$Tagged$Tagged = function (a) {
	return {$: 'Tagged', a: a};
};
var $joneshf$elm_tagged$Tagged$tag = $joneshf$elm_tagged$Tagged$Tagged;
var $author$project$Main$init = F3(
	function (flags, url, navKey) {
		var route = $author$project$Route$getRoute(url);
		var userStatus = $author$project$Main$routeToInitUserStatus(route);
		return _Utils_Tuple2(
			{
				chatStatus: {
					chatId: $joneshf$elm_tagged$Tagged$tag(''),
					err: $elm$core$Maybe$Nothing,
					hasManualScrolledUp: false,
					input: $joneshf$elm_tagged$Tagged$tag(''),
					isInputFocused: false,
					joinCount: 0,
					maxJoinCount: $elm$core$Maybe$Nothing,
					msgs: _List_Nil,
					myUserId: -1,
					shouldHintNewMsg: false,
					users: $elm$core$Dict$empty
				},
				dispChatMaxJoinCountInput: '2',
				isShiftHeld: false,
				isWsReady: false,
				letterPersistInput: true,
				letterRawInput: {body: '', maxReadCount: '1'},
				letterStatus: {read: $author$project$Letter$Init, write: $author$project$Letter$NotSent},
				navKey: navKey,
				newNameInput: '',
				persistChatMaxJoinCountInput: '2',
				route: route,
				tempResp: '',
				userStatus: userStatus,
				viewport: {
					scene: {height: 1080, width: 1920},
					viewport: {height: 1080, width: 1920, x: 0, y: 0}
				}
			},
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$author$project$Main$getViewportCmd,
						function () {
						switch (route.$) {
							case 'Chat':
								var chatIdStr = route.a;
								return $author$project$Main$port_InitWs(chatIdStr);
							case 'ReadLetter':
								var letterId = route.a;
								return $author$project$Main$getLetterReq(letterId);
							default:
								return $elm$core$Platform$Cmd$none;
						}
					}()
					])));
	});
var $author$project$CoreTypes$OnKeyDown = function (a) {
	return {$: 'OnKeyDown', a: a};
};
var $author$project$CoreTypes$OnKeyUp = function (a) {
	return {$: 'OnKeyUp', a: a};
};
var $author$project$CoreTypes$OnWindowResized = {$: 'OnWindowResized'};
var $author$project$CoreTypes$OnWsMsg = function (a) {
	return {$: 'OnWsMsg', a: a};
};
var $author$project$CoreTypes$OnWsReady = function (a) {
	return {$: 'OnWsReady', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $elm$browser$Browser$Events$Window = {$: 'Window'};
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		$elm$browser$Browser$Events$Window,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$port_RecvWsMsg = _Platform_incomingPort('port_RecvWsMsg', $elm$json$Json$Decode$string);
var $author$project$Main$port_WsReady = _Platform_incomingPort('port_WsReady', $elm$json$Json$Decode$string);
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$port_WsReady($author$project$CoreTypes$OnWsReady),
				$author$project$Main$port_RecvWsMsg($author$project$CoreTypes$OnWsMsg),
				$elm$browser$Browser$Events$onResize(
				F2(
					function (_v1, _v2) {
						return $author$project$CoreTypes$OnWindowResized;
					})),
				$elm$browser$Browser$Events$onKeyDown(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$CoreTypes$OnKeyDown,
					A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string))),
				$elm$browser$Browser$Events$onKeyUp(
				A2(
					$elm$json$Json$Decode$map,
					$author$project$CoreTypes$OnKeyUp,
					A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)))
			]));
};
var $author$project$CoreTypes$ChatMsgsViewEvent = function (a) {
	return {$: 'ChatMsgsViewEvent', a: a};
};
var $author$project$Chat$Content = {$: 'Content'};
var $author$project$Letter$Got = function (a) {
	return {$: 'Got', a: a};
};
var $author$project$CoreTypes$GotLetterSendResp = function (a) {
	return {$: 'GotLetterSendResp', a: a};
};
var $author$project$UserStatus$GotPersistChatIdLetter = function (a) {
	return {$: 'GotPersistChatIdLetter', a: a};
};
var $author$project$Letter$GotResp = function (a) {
	return {$: 'GotResp', a: a};
};
var $author$project$CoreTypes$GotSpawnDispChatResp = function (a) {
	return {$: 'GotSpawnDispChatResp', a: a};
};
var $author$project$CoreTypes$GotSpawnPersistChatResp = function (a) {
	return {$: 'GotSpawnPersistChatResp', a: a};
};
var $author$project$Chat$GotViewport = function (a) {
	return {$: 'GotViewport', a: a};
};
var $author$project$Chat$Join = {$: 'Join'};
var $author$project$Letter$Sent = function (a) {
	return {$: 'Sent', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $author$project$Chat$autoScrollMargin = 30;
var $author$project$Common$Urls$backendSpawnDispChatUrl = A2(
	$author$project$Common$Urls$mkBackendUrl,
	_List_fromArray(
		['spawn-disposable-chat']),
	_List_Nil);
var $author$project$Common$Urls$backendSpawnPersistChatUrl = A2(
	$author$project$Common$Urls$mkBackendUrl,
	_List_fromArray(
		['spawn-persistent-chat']),
	_List_Nil);
var $author$project$Common$Urls$backendWriteLetterUrl = A2(
	$author$project$Common$Urls$mkBackendUrl,
	_List_fromArray(
		['write-letter']),
	_List_Nil);
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$http$Http$expectString = function (toMsg) {
	return A2(
		$elm$http$Http$expectStringResponse,
		toMsg,
		$elm$http$Http$resolve($elm$core$Result$Ok));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Common$Urls$frontendChatUrl = function (chatId) {
	return A2(
		$elm$url$Url$Builder$absolute,
		_List_fromArray(
			['chat', chatId]),
		_List_Nil);
};
var $elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var $author$project$Utils$Utils$hasManualScrolledUp = F2(
	function (viewport, margin) {
		return (_Utils_cmp((viewport.viewport.y + viewport.viewport.height) + margin, viewport.scene.height) < 0) ? true : false;
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Chat$mkWsMsg = F2(
	function (msgType, msgBody) {
		return A2(
			$elm$json$Json$Encode$encode,
			0,
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'msgType',
						$elm$json$Json$Encode$string(msgType)),
						_Utils_Tuple2(
						'msgBody',
						$elm$json$Json$Encode$string(msgBody))
					])));
	});
var $joneshf$elm_tagged$Tagged$untag = function (_v0) {
	var x = _v0.a;
	return x;
};
var $author$project$Chat$mkJoinMsg = A2(
	$elm$core$Basics$composeL,
	$author$project$Chat$mkWsMsg('join'),
	$joneshf$elm_tagged$Tagged$untag);
var $author$project$Chat$mkNameChangeMsg = A2(
	$elm$core$Basics$composeL,
	$author$project$Chat$mkWsMsg('nameChange'),
	$joneshf$elm_tagged$Tagged$untag);
var $author$project$Views$Chat$msgsViewHtmlId = 'chat-msgs-view';
var $elm$core$Basics$not = _Basics_not;
var $author$project$Main$port_DebugLog = _Platform_outgoingPort('port_DebugLog', $elm$json$Json$Encode$string);
var $author$project$Main$port_SendWsMsg = _Platform_outgoingPort('port_SendWsMsg', $elm$json$Json$Encode$string);
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $author$project$Chat$mkContentMsg = A2(
	$elm$core$Basics$composeL,
	$author$project$Chat$mkWsMsg('content'),
	$joneshf$elm_tagged$Tagged$untag);
var $author$project$Main$sendChatMsg = function (msgBody) {
	return $elm$core$String$isEmpty(
		$joneshf$elm_tagged$Tagged$untag(msgBody)) ? $elm$core$Platform$Cmd$none : $author$project$Main$port_SendWsMsg(
		$author$project$Chat$mkContentMsg(msgBody));
};
var $author$project$Chat$TriedSnapScroll = function (a) {
	return {$: 'TriedSnapScroll', a: a};
};
var $elm$browser$Browser$Dom$setViewportOf = _Browser_setViewportOf;
var $author$project$Main$snapScrollChatMsgsView = A3(
	$elm$core$Basics$apL,
	$elm$core$Task$attempt,
	A2($elm$core$Basics$composeL, $author$project$CoreTypes$ChatMsgsViewEvent, $author$project$Chat$TriedSnapScroll),
	A2(
		$elm$core$Task$andThen,
		function (viewport) {
			return A3($elm$browser$Browser$Dom$setViewportOf, $author$project$Views$Chat$msgsViewHtmlId, 0, viewport.scene.height);
		},
		$elm$browser$Browser$Dom$getViewportOf($author$project$Views$Chat$msgsViewHtmlId)));
var $author$project$Utils$Types$Bad = function (a) {
	return {$: 'Bad', a: a};
};
var $author$project$Utils$Types$Good = function (a) {
	return {$: 'Good', a: a};
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Utils$Types$strToPosIntInput = function (str) {
	var _v0 = $elm$core$String$toInt(str);
	if (_v0.$ === 'Nothing') {
		return $author$project$Utils$Types$Bad(str);
	} else {
		var n = _v0.a;
		return (n >= 1) ? $author$project$Utils$Types$Good(n) : $author$project$Utils$Types$Bad(str);
	}
};
var $elm$http$Http$stringBody = _Http_pair;
var $elm$core$Debug$toString = _Debug_toString;
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $elm$core$Debug$todo = _Debug_todo;
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $elm$core$String$endsWith = _String_endsWith;
var $elm_community$string_extra$String$Extra$unsurround = F2(
	function (wrapper, string) {
		if (A2($elm$core$String$startsWith, wrapper, string) && A2($elm$core$String$endsWith, wrapper, string)) {
			var length = $elm$core$String$length(wrapper);
			return A2(
				$elm$core$String$dropRight,
				length,
				A2($elm$core$String$dropLeft, length, string));
		} else {
			return string;
		}
	});
var $elm_community$string_extra$String$Extra$unquote = function (string) {
	return A2($elm_community$string_extra$String$Extra$unsurround, '\"', string);
};
var $author$project$Letter$BadMaxReadCount = {$: 'BadMaxReadCount'};
var $author$project$Letter$EmptyBody = {$: 'EmptyBody'};
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $author$project$Letter$validateInput = function (input) {
	return A2(
		$elm$core$Result$andThen,
		function (input_) {
			var _v0 = $author$project$Utils$Types$strToPosIntInput(input_.maxReadCount);
			if (_v0.$ === 'Bad') {
				return $elm$core$Result$Err($author$project$Letter$BadMaxReadCount);
			} else {
				var posInt = _v0.a;
				return $elm$core$Result$Ok(
					{body: input_.body, maxReadCount: posInt});
			}
		},
		function (input_) {
			return $elm$core$String$isEmpty(input_.body) ? $elm$core$Result$Err($author$project$Letter$EmptyBody) : $elm$core$Result$Ok(input_);
		}(input));
};
var $author$project$Chat$ChatMsgMeta_ = function (a) {
	return {$: 'ChatMsgMeta_', a: a};
};
var $author$project$Chat$CtrlMsg_ = function (a) {
	return {$: 'CtrlMsg_', a: a};
};
var $author$project$Chat$MsgHistory_ = function (a) {
	return {$: 'MsgHistory_', a: a};
};
var $author$project$Chat$UserIdMsg_ = function (a) {
	return {$: 'UserIdMsg_', a: a};
};
var $author$project$Chat$ChatMsgMeta = F4(
	function (msgFromClient, userId, username, posixTimeSec) {
		return {msgFromClient: msgFromClient, posixTimeSec: posixTimeSec, userId: userId, username: username};
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Chat$Leave = {$: 'Leave'};
var $author$project$Chat$MsgFromClient = F2(
	function (msgType, msgBody) {
		return {msgBody: msgBody, msgType: msgType};
	});
var $author$project$Chat$NameChange = {$: 'NameChange'};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Chat$msgFromClientDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Chat$MsgFromClient,
	A2(
		$elm$json$Json$Decode$andThen,
		function (str) {
			return (str === 'content') ? $elm$json$Json$Decode$succeed($author$project$Chat$Content) : ((str === 'join') ? $elm$json$Json$Decode$succeed($author$project$Chat$Join) : ((str === 'nameChange') ? $elm$json$Json$Decode$succeed($author$project$Chat$NameChange) : ((str === 'leave') ? $elm$json$Json$Decode$succeed($author$project$Chat$Leave) : $elm$json$Json$Decode$fail('Invalid MsgType'))));
		},
		A2($elm$json$Json$Decode$field, 'msgType', $elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$map,
		$joneshf$elm_tagged$Tagged$tag,
		A2($elm$json$Json$Decode$field, 'msgBody', $elm$json$Json$Decode$string)));
var $author$project$Chat$chatMsgMetaDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Chat$ChatMsgMeta,
	A2($elm$json$Json$Decode$field, 'msgFromClient', $author$project$Chat$msgFromClientDecoder),
	A2($elm$json$Json$Decode$field, 'userId', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'username', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'posixTimeSec', $elm$json$Json$Decode$int));
var $author$project$Chat$ctrlMsgTypeDecoder = A2($elm$json$Json$Decode$field, 'msgType', $elm$json$Json$Decode$string);
var $author$project$Chat$Err_ = function (a) {
	return {$: 'Err_', a: a};
};
var $author$project$Chat$MaxJoined = {$: 'MaxJoined'};
var $author$project$Chat$NotFound = {$: 'NotFound'};
var $author$project$Chat$errCtrlMsgDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Chat$Err_,
	A2(
		$elm$json$Json$Decode$andThen,
		function (str) {
			return (str === 'maxJoined') ? $elm$json$Json$Decode$succeed($author$project$Chat$MaxJoined) : ((str === 'notFound') ? $elm$json$Json$Decode$succeed($author$project$Chat$NotFound) : $elm$json$Json$Decode$fail('Invalid error ctrl msg type'));
		},
		A2($elm$json$Json$Decode$field, 'msgBody', $elm$json$Json$Decode$string)));
var $author$project$Utils$Utils$is = F2(
	function (x, y) {
		return _Utils_eq(x, y);
	});
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm_community$json_extra$Json$Decode$Extra$when = F3(
	function (checkDecoder, check, passDecoder) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (checkVal) {
				return check(checkVal) ? passDecoder : $elm$json$Json$Decode$fail('Check failed with input');
			},
			checkDecoder);
	});
var $author$project$Chat$ctrlMsgDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A3(
			$elm_community$json_extra$Json$Decode$Extra$when,
			$author$project$Chat$ctrlMsgTypeDecoder,
			$author$project$Utils$Utils$is('err'),
			$author$project$Chat$errCtrlMsgDecoder)
		]));
var $author$project$Chat$MsgHistory = F3(
	function (msgs, users, maxJoinCount) {
		return {maxJoinCount: maxJoinCount, msgs: msgs, users: users};
	});
var $elm_community$json_extra$Json$Decode$Extra$decodeDictFromTuples = F2(
	function (keyDecoder, tuples) {
		if (!tuples.b) {
			return $elm$json$Json$Decode$succeed($elm$core$Dict$empty);
		} else {
			var _v1 = tuples.a;
			var strKey = _v1.a;
			var value = _v1.b;
			var rest = tuples.b;
			var _v2 = A2($elm$json$Json$Decode$decodeString, keyDecoder, strKey);
			if (_v2.$ === 'Ok') {
				var key = _v2.a;
				return A2(
					$elm$json$Json$Decode$andThen,
					A2(
						$elm$core$Basics$composeR,
						A2($elm$core$Dict$insert, key, value),
						$elm$json$Json$Decode$succeed),
					A2($elm_community$json_extra$Json$Decode$Extra$decodeDictFromTuples, keyDecoder, rest));
			} else {
				var error = _v2.a;
				return $elm$json$Json$Decode$fail(
					$elm$json$Json$Decode$errorToString(error));
			}
		}
	});
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm_community$json_extra$Json$Decode$Extra$dict2 = F2(
	function (keyDecoder, valueDecoder) {
		return A2(
			$elm$json$Json$Decode$andThen,
			$elm_community$json_extra$Json$Decode$Extra$decodeDictFromTuples(keyDecoder),
			$elm$json$Json$Decode$keyValuePairs(valueDecoder));
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Chat$msgHistoryDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Chat$MsgHistory,
	A2(
		$elm$json$Json$Decode$field,
		'msgs',
		$elm$json$Json$Decode$list($author$project$Chat$chatMsgMetaDecoder)),
	A2(
		$elm$json$Json$Decode$field,
		'users',
		A2($elm_community$json_extra$Json$Decode$Extra$dict2, $elm$json$Json$Decode$int, $elm$json$Json$Decode$string)),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$field, 'maxJoinCount', $elm$json$Json$Decode$int)));
var $author$project$Chat$UserIdMsg = function (yourUserId) {
	return {yourUserId: yourUserId};
};
var $author$project$Chat$userIdMsgDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Chat$UserIdMsg,
	A2($elm$json$Json$Decode$field, 'yourUserId', $elm$json$Json$Decode$int));
var $author$project$Chat$wsMsgDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2($elm$json$Json$Decode$map, $author$project$Chat$ChatMsgMeta_, $author$project$Chat$chatMsgMetaDecoder),
			A2($elm$json$Json$Decode$map, $author$project$Chat$CtrlMsg_, $author$project$Chat$ctrlMsgDecoder),
			A2($elm$json$Json$Decode$map, $author$project$Chat$MsgHistory_, $author$project$Chat$msgHistoryDecoder),
			A2($elm$json$Json$Decode$map, $author$project$Chat$UserIdMsg_, $author$project$Chat$userIdMsgDecoder)
		]));
var $author$project$Main$update = F2(
	function (msg, model) {
		var letterRawInput = model.letterRawInput;
		var letterStatus = model.letterStatus;
		var chatStatus = model.chatStatus;
		switch (msg.$) {
			case 'UrlRequested':
				var req = msg.a;
				if (req.$ === 'Internal') {
					var url = req.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.navKey,
							$elm$url$Url$toString(url)));
				} else {
					var urlStr = req.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(urlStr));
				}
			case 'UrlChanged':
				var url = msg.a;
				var route = $author$project$Route$getRoute(url);
				var userStatus = $author$project$Main$routeToInitUserStatus(route);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{route: route, userStatus: userStatus}),
					function () {
						switch (route.$) {
							case 'Chat':
								var chatIdStr = route.a;
								return $author$project$Main$port_InitWs(chatIdStr);
							case 'ReadLetter':
								var letterId = route.a;
								return $author$project$Main$getLetterReq(letterId);
							default:
								return $elm$core$Platform$Cmd$none;
						}
					}());
			case 'GotViewport':
				var viewport = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{viewport: viewport}),
					$elm$core$Platform$Cmd$none);
			case 'GotReadLetterResp':
				var result = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							letterStatus: _Utils_update(
								letterStatus,
								{
									read: $author$project$Letter$Got(result)
								})
						}),
					$elm$core$Platform$Cmd$none);
			case 'LetterInput':
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							letterRawInput: _Utils_update(
								letterRawInput,
								{body: str})
						}),
					$elm$core$Platform$Cmd$none);
			case 'LetterMaxReadCountInput':
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							letterRawInput: _Utils_update(
								letterRawInput,
								{maxReadCount: str})
						}),
					$elm$core$Platform$Cmd$none);
			case 'LetterPersistInput':
				var input = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{letterPersistInput: input}),
					$elm$core$Platform$Cmd$none);
			case 'LetterSend':
				var _v3 = $author$project$Letter$validateInput(letterRawInput);
				if (_v3.$ === 'Err') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var goodInput = _v3.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								letterRawInput: _Utils_update(
									letterRawInput,
									{body: '', maxReadCount: '1'}),
								letterStatus: _Utils_update(
									letterStatus,
									{
										write: $author$project$Letter$Sent(
											{maxReadCount: goodInput.maxReadCount})
									})
							}),
						$elm$http$Http$request(
							{
								body: $elm$http$Http$jsonBody(
									$elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'body',
												$elm$json$Json$Encode$string(goodInput.body)),
												_Utils_Tuple2(
												'maxReadCount',
												$elm$json$Json$Encode$int(goodInput.maxReadCount)),
												_Utils_Tuple2(
												'persist',
												$elm$json$Json$Encode$bool(model.letterPersistInput))
											]))),
								expect: $elm$http$Http$expectString($author$project$CoreTypes$GotLetterSendResp),
								headers: _List_Nil,
								method: 'PUT',
								timeout: $elm$core$Maybe$Nothing,
								tracker: $elm$core$Maybe$Nothing,
								url: $author$project$Common$Urls$backendWriteLetterUrl
							}));
				}
			case 'GotLetterSendResp':
				var result = msg.a;
				var _v4 = model.letterStatus.write;
				if (_v4.$ === 'Sent') {
					var info = _v4.a;
					var newModel = function () {
						if (result.$ === 'Err') {
							var err = result.a;
							return _Utils_update(
								model,
								{
									letterStatus: _Utils_update(
										letterStatus,
										{
											write: $author$project$Letter$GotResp(
												$elm$core$Result$Err(err))
										})
								});
						} else {
							var letterId = result.a;
							return _Utils_update(
								model,
								{
									letterStatus: _Utils_update(
										letterStatus,
										{
											write: $author$project$Letter$GotResp(
												$elm$core$Result$Ok(
													{id: letterId, maxReadCount: info.maxReadCount}))
										})
								});
						}
					}();
					return _Utils_Tuple2(newModel, $elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						model,
						_Debug_todo(
							'Main',
							{
								start: {line: 234, column: 23},
								end: {line: 234, column: 33}
							})('Incorrect LetterStatus when letter send is responeded.'));
				}
			case 'DispChatMaxJoinCountInput':
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{dispChatMaxJoinCountInput: str}),
					$elm$core$Platform$Cmd$none);
			case 'SpawnDispChat':
				var _v6 = $author$project$Utils$Types$strToPosIntInput(model.dispChatMaxJoinCountInput);
				if (_v6.$ === 'Bad') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var posInt = _v6.a;
					return _Utils_Tuple2(
						model,
						$elm$http$Http$request(
							{
								body: A2(
									$elm$http$Http$stringBody,
									'text/plain;charset=utf-8',
									$elm$core$String$fromInt(posInt)),
								expect: $elm$http$Http$expectString($author$project$CoreTypes$GotSpawnDispChatResp),
								headers: _List_Nil,
								method: 'PUT',
								timeout: $elm$core$Maybe$Nothing,
								tracker: $elm$core$Maybe$Nothing,
								url: $author$project$Common$Urls$backendSpawnDispChatUrl
							}));
				}
			case 'GotSpawnDispChatResp':
				var result = msg.a;
				if (result.$ === 'Err') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var chatId = result.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								chatStatus: _Utils_update(
									chatStatus,
									{
										chatId: $joneshf$elm_tagged$Tagged$tag(
											$elm_community$string_extra$String$Extra$unquote(chatId))
									})
							}),
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.navKey,
							$author$project$Common$Urls$frontendChatUrl(
								$elm_community$string_extra$String$Extra$unquote(chatId))));
				}
			case 'PersistChatMaxJoinCountInput':
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{persistChatMaxJoinCountInput: str}),
					$elm$core$Platform$Cmd$none);
			case 'SpawnPersistChat':
				return _Utils_Tuple2(
					model,
					function () {
						var _v8 = $author$project$Utils$Types$strToPosIntInput(model.persistChatMaxJoinCountInput);
						if (_v8.$ === 'Bad') {
							return $elm$core$Platform$Cmd$none;
						} else {
							var posInt = _v8.a;
							return $elm$http$Http$request(
								{
									body: A2(
										$elm$http$Http$stringBody,
										'text/plain;charset=utf-8',
										$elm$core$String$fromInt(posInt)),
									expect: $elm$http$Http$expectString($author$project$CoreTypes$GotSpawnPersistChatResp),
									headers: _List_Nil,
									method: 'PUT',
									timeout: $elm$core$Maybe$Nothing,
									tracker: $elm$core$Maybe$Nothing,
									url: $author$project$Common$Urls$backendSpawnPersistChatUrl
								});
						}
					}());
			case 'GotSpawnPersistChatResp':
				var result = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							userStatus: $author$project$UserStatus$GotPersistChatIdLetter(result)
						}),
					$elm$core$Platform$Cmd$none);
			case 'MessageInput':
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							chatStatus: _Utils_update(
								chatStatus,
								{
									input: $joneshf$elm_tagged$Tagged$tag(str)
								})
						}),
					$elm$core$Platform$Cmd$none);
			case 'MessageSend':
				return _Utils_Tuple2(
					model,
					$author$project$Main$sendChatMsg(model.chatStatus.input));
			case 'NewNameInput':
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{newNameInput: str}),
					$elm$core$Platform$Cmd$none);
			case 'NameChange':
				return $elm$core$String$isEmpty(model.newNameInput) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{newNameInput: ''}),
					$author$project$Main$port_SendWsMsg(
						$author$project$Chat$mkNameChangeMsg(
							$joneshf$elm_tagged$Tagged$tag(model.newNameInput))));
			case 'OnWsReady':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{isWsReady: true}),
					function () {
						var _v9 = model.route;
						if (_v9.$ === 'Chat') {
							var chatIdStr = _v9.a;
							return $author$project$Main$port_SendWsMsg(
								$author$project$Chat$mkJoinMsg(
									$joneshf$elm_tagged$Tagged$tag(chatIdStr)));
						} else {
							return $elm$core$Platform$Cmd$none;
						}
					}());
			case 'OnWsMsg':
				var str = msg.a;
				var _v10 = A2($elm$json$Json$Decode$decodeString, $author$project$Chat$wsMsgDecoder, str);
				if (_v10.$ === 'Err') {
					return _Utils_Tuple2(
						model,
						_Debug_todo(
							'Main',
							{
								start: {line: 334, column: 30},
								end: {line: 334, column: 40}
							})('hi'));
				} else {
					var wsMsg = _v10.a;
					switch (wsMsg.$) {
						case 'ChatMsgMeta_':
							var chatMsgMeta = wsMsg.a;
							var senderName = chatMsgMeta.username;
							var senderId = chatMsgMeta.userId;
							var oldUsers = model.chatStatus.users;
							var msgFromClient = chatMsgMeta.msgFromClient;
							var newUsers = function () {
								var _v12 = msgFromClient.msgType;
								switch (_v12.$) {
									case 'Join':
										return A3($elm$core$Dict$insert, senderId, senderName, oldUsers);
									case 'NameChange':
										return A3(
											$elm$core$Dict$insert,
											senderId,
											$joneshf$elm_tagged$Tagged$untag(msgFromClient.msgBody),
											oldUsers);
									case 'Leave':
										return A2($elm$core$Dict$remove, senderId, oldUsers);
									default:
										return model.chatStatus.users;
								}
							}();
							var isMyMsg = _Utils_eq(chatMsgMeta.userId, model.chatStatus.myUserId);
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										chatStatus: _Utils_update(
											chatStatus,
											{
												input: (isMyMsg && _Utils_eq(msgFromClient.msgType, $author$project$Chat$Content)) ? $joneshf$elm_tagged$Tagged$tag('') : model.chatStatus.input,
												joinCount: _Utils_eq(msgFromClient.msgType, $author$project$Chat$Join) ? (senderId + 1) : model.chatStatus.joinCount,
												msgs: _Utils_ap(
													chatStatus.msgs,
													_List_fromArray(
														[chatMsgMeta])),
												shouldHintNewMsg: model.chatStatus.shouldHintNewMsg || (model.chatStatus.hasManualScrolledUp && (!isMyMsg)),
												users: newUsers
											})
									}),
								(!model.chatStatus.hasManualScrolledUp) ? $author$project$Main$snapScrollChatMsgsView : $elm$core$Platform$Cmd$none);
						case 'CtrlMsg_':
							var ctrlMsg = wsMsg.a;
							var err = ctrlMsg.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										chatStatus: _Utils_update(
											chatStatus,
											{
												err: $elm$core$Maybe$Just(err)
											})
									}),
								$elm$core$Platform$Cmd$none);
						case 'MsgHistory_':
							var msgHistory = wsMsg.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										chatStatus: _Utils_update(
											chatStatus,
											{maxJoinCount: msgHistory.maxJoinCount, msgs: msgHistory.msgs, users: msgHistory.users})
									}),
								$elm$core$Platform$Cmd$none);
						default:
							var userIdMsg = wsMsg.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										chatStatus: _Utils_update(
											chatStatus,
											{myUserId: userIdMsg.yourUserId})
									}),
								$elm$core$Platform$Cmd$none);
					}
				}
			case 'ChatMsgsViewEvent':
				var event = msg.a;
				switch (event.$) {
					case 'TriedSnapScroll':
						var result = event.a;
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 'OnManualScrolled':
						return _Utils_Tuple2(
							model,
							A2(
								$elm$core$Task$attempt,
								A2($elm$core$Basics$composeL, $author$project$CoreTypes$ChatMsgsViewEvent, $author$project$Chat$GotViewport),
								$elm$browser$Browser$Dom$getViewportOf($author$project$Views$Chat$msgsViewHtmlId)));
					case 'GotViewport':
						var result = event.a;
						if (result.$ === 'Err') {
							var err = result.a;
							return _Utils_Tuple2(
								model,
								$author$project$Main$port_DebugLog(
									$elm$core$Debug$toString(err)));
						} else {
							var viewport = result.a;
							var logViewport = $author$project$Main$port_DebugLog(
								$elm$core$String$fromFloat(viewport.viewport.y) + (', ' + ($elm$core$String$fromFloat(viewport.viewport.height) + (', ' + $elm$core$String$fromFloat(viewport.scene.height)))));
							var hasManualScrolledUp = A2($author$project$Utils$Utils$hasManualScrolledUp, viewport, $author$project$Chat$autoScrollMargin);
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										chatStatus: _Utils_update(
											chatStatus,
											{hasManualScrolledUp: hasManualScrolledUp, shouldHintNewMsg: model.chatStatus.shouldHintNewMsg && hasManualScrolledUp})
									}),
								$elm$core$Platform$Cmd$none);
						}
					default:
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									chatStatus: _Utils_update(
										chatStatus,
										{hasManualScrolledUp: false, shouldHintNewMsg: false})
								}),
							$author$project$Main$snapScrollChatMsgsView);
				}
			case 'OnChatInputFocal':
				var isFocused = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							chatStatus: _Utils_update(
								chatStatus,
								{isInputFocused: isFocused})
						}),
					$elm$core$Platform$Cmd$none);
			case 'OnWindowResized':
				return _Utils_Tuple2(model, $author$project$Main$getViewportCmd);
			case 'OnKeyDown':
				var key = msg.a;
				switch (key) {
					case 'Enter':
						return _Utils_Tuple2(
							model,
							((!model.isShiftHeld) && model.chatStatus.isInputFocused) ? $author$project$Main$sendChatMsg(model.chatStatus.input) : $elm$core$Platform$Cmd$none);
					case 'Shift':
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{isShiftHeld: true}),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'OnKeyUp':
				var key = msg.a;
				if (key === 'Shift') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{isShiftHeld: false}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Common$Urls$aboutUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['about']),
	_List_Nil);
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 'Rgba', a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var $author$project$Common$Colors$bgColor = A3($mdgriffith$elm_ui$Element$rgb255, 20, 30, 36);
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 'Colored', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 'StyleClass', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 'Flag', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 'Second', a: a};
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
var $elm$core$Basics$round = _Basics_round;
var $mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return $elm$core$String$fromInt(
		$elm$core$Basics$round(x * 255));
};
var $mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return $mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var $mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var $mdgriffith$elm_ui$Internal$Flag$fontColor = $mdgriffith$elm_ui$Internal$Flag$flag(14);
var $mdgriffith$elm_ui$Element$Font$color = function (fontColor) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(fontColor),
			'color',
			fontColor));
};
var $author$project$Common$Urls$configChatUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['config-chat']),
	_List_Nil);
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 'Unkeyed', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsEl = {$: 'AsEl'};
var $mdgriffith$elm_ui$Internal$Model$asEl = $mdgriffith$elm_ui$Internal$Model$AsEl;
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 'Generic'};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 'NoNearbyChildren'};
var $mdgriffith$elm_ui$Internal$Style$classes = {above: 'a', active: 'atv', alignBottom: 'ab', alignCenterX: 'cx', alignCenterY: 'cy', alignContainerBottom: 'acb', alignContainerCenterX: 'accx', alignContainerCenterY: 'accy', alignContainerRight: 'acr', alignLeft: 'al', alignRight: 'ar', alignTop: 'at', alignedHorizontally: 'ah', alignedVertically: 'av', any: 's', behind: 'bh', below: 'b', bold: 'w7', borderDashed: 'bd', borderDotted: 'bdt', borderNone: 'bn', borderSolid: 'bs', capturePointerEvents: 'cpe', clip: 'cp', clipX: 'cpx', clipY: 'cpy', column: 'c', container: 'ctr', contentBottom: 'cb', contentCenterX: 'ccx', contentCenterY: 'ccy', contentLeft: 'cl', contentRight: 'cr', contentTop: 'ct', cursorPointer: 'cptr', cursorText: 'ctxt', focus: 'fcs', focusedWithin: 'focus-within', fullSize: 'fs', grid: 'g', hasBehind: 'hbh', heightContent: 'hc', heightExact: 'he', heightFill: 'hf', heightFillPortion: 'hfp', hover: 'hv', imageContainer: 'ic', inFront: 'fr', inputLabel: 'lbl', inputMultiline: 'iml', inputMultilineFiller: 'imlf', inputMultilineParent: 'imlp', inputMultilineWrapper: 'implw', inputText: 'it', italic: 'i', link: 'lnk', nearby: 'nb', noTextSelection: 'notxt', onLeft: 'ol', onRight: 'or', opaque: 'oq', overflowHidden: 'oh', page: 'pg', paragraph: 'p', passPointerEvents: 'ppe', root: 'ui', row: 'r', scrollbars: 'sb', scrollbarsX: 'sbx', scrollbarsY: 'sby', seButton: 'sbt', single: 'e', sizeByCapital: 'cap', spaceEvenly: 'sev', strike: 'sk', text: 't', textCenter: 'tc', textExtraBold: 'w8', textExtraLight: 'w2', textHeavy: 'w9', textJustify: 'tj', textJustifyAll: 'tja', textLeft: 'tl', textLight: 'w3', textMedium: 'w5', textNormalWeight: 'w4', textRight: 'tr', textSemiBold: 'w6', textThin: 'w1', textUnitalicized: 'tun', transition: 'ts', transparent: 'clr', underline: 'u', widthContent: 'wc', widthExact: 'we', widthFill: 'wf', widthFillPortion: 'wfp', wrapped: 'wrp'};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.column);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.grid);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.page);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.paragraph);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.row);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.single);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context.$) {
		case 'AsRow':
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 'AsColumn':
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 'AsEl':
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 'AsGrid':
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 'AsParagraph':
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 'Keyed', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 'NoStyleSheet'};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 'Styled', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 'NoNearbyChildren':
				return existing;
			case 'ChildrenBehind':
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 'ChildrenInFront':
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 'NoNearbyChildren':
				return existing;
			case 'ChildrenBehind':
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 'ChildrenInFront':
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = {$: 'AsParagraph'};
var $mdgriffith$elm_ui$Internal$Model$asParagraph = $mdgriffith$elm_ui$Internal$Model$AsParagraph;
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 'Px':
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 'Content':
			return 'auto';
		case 'Fill':
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 'Min':
			var min = x.a;
			var len = x.b;
			return 'min' + ($elm$core$String$fromInt(min) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + ($elm$core$String$fromInt(max) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 'Untransformed':
			return $elm$core$Maybe$Nothing;
		case 'Moved':
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'mv-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			return $elm$core$Maybe$Just(
				'tfrm-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 'Shadows':
			var name = style.a;
			return name;
		case 'Transparency':
			var name = style.a;
			var o = style.b;
			return name;
		case 'Style':
			var _class = style.a;
			return _class;
		case 'FontFamily':
			var name = style.a;
			return name;
		case 'FontSize':
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 'Single':
			var _class = style.a;
			return _class;
		case 'Colored':
			var _class = style.a;
			return _class;
		case 'SpacingStyle':
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 'PaddingStyle':
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 'BorderWidth':
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 'GridTemplateStyle':
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.rows)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.columns)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.b)))))));
		case 'GridPosition':
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.row) + ('-' + ($elm$core$String$fromInt(pos.col) + ('-' + ($elm$core$String$fromInt(pos.width) + ('-' + $elm$core$String$fromInt(pos.height)))))));
		case 'PseudoSelector':
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector.$) {
					case 'Focus':
						return 'fs';
					case 'Hover':
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (sty) {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_v1 === '') {
							return '';
						} else {
							var styleName = _v1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				$mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = $mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2($elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2($elm$core$Set$insert, styleName, cache),
			A2($elm$core$List$cons, style, existing));
	});
var $mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 'Property', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 'Style', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $mdgriffith$elm_ui$Internal$Model$formatColor = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return 'rgba(' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(red * 255)) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(green * 255))) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(blue * 255))) + (',' + ($elm$core$String$fromFloat(alpha) + ')')))));
};
var $mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.inset ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.offset.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.offset.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.blur) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.size) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.color))
				])));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.focusedWithin) + ':focus-within',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.borderColor),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.backgroundColor),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										blur: shadow.blur,
										color: shadow.color,
										inset: false,
										offset: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.offset)),
										size: shadow.size
									}));
						},
						focus.shadow),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ':focus .focusable, ') + (($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + '.focusable:focus, ') + ('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ' .focusable-thumb'))),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.borderColor),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.backgroundColor),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										blur: shadow.blur,
										color: shadow.color,
										inset: false,
										offset: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.offset)),
										size: shadow.size
									}));
						},
						focus.shadow),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					])))
		]);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Style$AllChildren = F2(
	function (a, b) {
		return {$: 'AllChildren', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 'Batch', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 'Child', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 'Class', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 'Descriptor', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = {$: 'Left'};
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 'Prop', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = {$: 'Right'};
var $mdgriffith$elm_ui$Internal$Style$Self = function (a) {
	return {$: 'Self', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 'Supports', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = function (a) {
	return {$: 'Content', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Bottom = {$: 'Bottom'};
var $mdgriffith$elm_ui$Internal$Style$CenterX = {$: 'CenterX'};
var $mdgriffith$elm_ui$Internal$Style$CenterY = {$: 'CenterY'};
var $mdgriffith$elm_ui$Internal$Style$Top = {$: 'Top'};
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[$mdgriffith$elm_ui$Internal$Style$Top, $mdgriffith$elm_ui$Internal$Style$Bottom, $mdgriffith$elm_ui$Internal$Style$Right, $mdgriffith$elm_ui$Internal$Style$Left, $mdgriffith$elm_ui$Internal$Style$CenterX, $mdgriffith$elm_ui$Internal$Style$CenterY]);
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc.a.$) {
		case 'Top':
			var _v1 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentTop);
		case 'Bottom':
			var _v2 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentBottom);
		case 'Right':
			var _v3 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentRight);
		case 'Left':
			var _v4 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentLeft);
		case 'CenterX':
			var _v5 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentCenterX);
		default:
			var _v6 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentCenterY);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc.a.$) {
		case 'Top':
			var _v1 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignTop);
		case 'Bottom':
			var _v2 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignBottom);
		case 'Right':
			var _v3 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignRight);
		case 'Left':
			var _v4 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignLeft);
		case 'CenterX':
			var _v5 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterX);
		default:
			var _v6 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY);
	}
};
var $mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _v0 = values(alignment);
		var content = _v0.a;
		var indiv = _v0.b;
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$contentName(
					$mdgriffith$elm_ui$Internal$Style$Content(alignment)),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(
							$mdgriffith$elm_ui$Internal$Style$Self(alignment)),
						indiv)
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.hasBehind),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.seButton),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightContent),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthContent),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment.$) {
				case 'Top':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 'Bottom':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 'Right':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 'Left':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 'CenterX':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var $mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(
							$mdgriffith$elm_ui$Internal$Style$Self(alignment)),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = {$: 'Above'};
var $mdgriffith$elm_ui$Internal$Style$Behind = {$: 'Behind'};
var $mdgriffith$elm_ui$Internal$Style$Below = {$: 'Below'};
var $mdgriffith$elm_ui$Internal$Style$OnLeft = {$: 'OnLeft'};
var $mdgriffith$elm_ui$Internal$Style$OnRight = {$: 'OnRight'};
var $mdgriffith$elm_ui$Internal$Style$Within = {$: 'Within'};
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = $mdgriffith$elm_ui$Internal$Style$Above;
	var _v0 = function () {
		switch (loc.$) {
			case 'Above':
				return _Utils_Tuple0;
			case 'Below':
				return _Utils_Tuple0;
			case 'OnRight':
				return _Utils_Tuple0;
			case 'OnLeft':
				return _Utils_Tuple0;
			case 'Within':
				return _Utils_Tuple0;
			default:
				return _Utils_Tuple0;
		}
	}();
	return _List_fromArray(
		[$mdgriffith$elm_ui$Internal$Style$Above, $mdgriffith$elm_ui$Internal$Style$Below, $mdgriffith$elm_ui$Internal$Style$OnRight, $mdgriffith$elm_ui$Internal$Style$OnLeft, $mdgriffith$elm_ui$Internal$Style$Within, $mdgriffith$elm_ui$Internal$Style$Behind]);
}();
var $mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.imageContainer))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'img',
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'max-height', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'object-fit', 'cover')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'img',
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'max-width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'object-fit', 'cover')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.root),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inFront),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.nearby),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.nearby),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc.$) {
							case 'Above':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.above),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'Below':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.below),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 'OnRight':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onRight),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'OnLeft':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onLeft),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'Within':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inFront),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.wrapped),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.noTextSelection),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cursorPointer),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cursorText),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.passPointerEvents),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.capturePointerEvents),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.transparent),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.opaque),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.hover, $mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.hover, $mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.focus, $mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.focus, $mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.active, $mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.active, $mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.transition),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.scrollbars),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.scrollbarsX),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.scrollbarsY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.column),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.clip),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.clipX),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.clipY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthContent),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderNone),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderDashed),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderDotted),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderSolid),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputText),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'inherit')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthExact),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.link),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.container),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterX),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterX),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 'Bottom':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 'Right':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 'Left':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 'CenterX':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.spaceEvenly),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'baseline')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.column),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0px'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', 'min-content'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightExact),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthContent),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 'Bottom':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 'Right':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 'CenterX':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.container),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.spaceEvenly),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.grid),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 'Bottom':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 'Right':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 'Left':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 'CenterX':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.page),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.any + ($mdgriffith$elm_ui$Internal$Style$selfName(
								$mdgriffith$elm_ui$Internal$Style$Self($mdgriffith$elm_ui$Internal$Style$Left)) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.any))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.any + ($mdgriffith$elm_ui$Internal$Style$selfName(
								$mdgriffith$elm_ui$Internal$Style$Self($mdgriffith$elm_ui$Internal$Style$Right)) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.any))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Bottom':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Right':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 'CenterX':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultiline),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineWrapper),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineParent),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineFiller),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.paragraph),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-wrap', 'break-word'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.hasBehind),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.paragraph),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								'::after',
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', 'none')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								'::before',
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', 'none')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthExact),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inFront),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.above),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.below),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onRight),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onLeft),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.column),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.grid),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Bottom':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Right':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 'CenterX':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textThin),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textExtraLight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textLight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textNormalWeight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textMedium),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textSemiBold),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bold),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textExtraBold),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textHeavy),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.italic),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.strike),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.underline),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.underline),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.strike)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textUnitalicized),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textJustify),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textJustifyAll),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textCenter),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textRight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textLeft),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var $mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var $mdgriffith$elm_ui$Internal$Style$commonValues = $elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			$elm$core$List$map,
			function (x) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + $elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							$elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 6)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 8, 32)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.any + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.any + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.container) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = function (a) {
	return {$: 'Intermediate', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return $mdgriffith$elm_ui$Internal$Style$Intermediate(
			{closing: closing, others: _List_Nil, props: _List_Nil, selector: selector});
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0.a;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 'Prop':
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								props: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.props)
							});
					case 'Supports':
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Style$Intermediate(
										{closing: '\n}', others: _List_Nil, props: props, selector: '@supports (' + (prop + (':' + (value + (') {' + parent.selector))))}),
									rendered.others)
							});
					case 'Adjacent':
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' + ' + selector), ''),
										adjRules),
									rendered.others)
							});
					case 'Child':
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' > ' + child), ''),
										childRules),
									rendered.others)
							});
					case 'AllChildren':
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' ' + child), ''),
										childRules),
									rendered.others)
							});
					case 'Descriptor':
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.selector, descriptor),
											''),
										descriptorRules),
									rendered.others)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector, ''),
										batched),
									rendered.others)
							});
				}
			});
		return $mdgriffith$elm_ui$Internal$Style$Intermediate(
			A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender));
	});
var $mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _v2 = rule.props;
		if (!_v2.b) {
			return '';
		} else {
			return rule.selector + ('{' + (renderValues(rule.props) + (rule.closing + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0.a;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.others)));
	};
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			renderIntermediate,
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v1, existing) {
						var name = _v1.a;
						var styleRules = _v1.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$mdgriffith$elm_ui$Internal$Style$renderRules,
								A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var $mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	$mdgriffith$elm_ui$Internal$Style$overrides,
	$mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap($mdgriffith$elm_ui$Internal$Style$baseSheet, $mdgriffith$elm_ui$Internal$Style$commonValues)));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $mdgriffith$elm_ui$Internal$Model$staticRoot = function (opts) {
	var _v0 = opts.mode;
	switch (_v0.$) {
		case 'Layout':
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'div',
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$elm$virtual_dom$VirtualDom$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$elm$virtual_dom$VirtualDom$text($mdgriffith$elm_ui$Internal$Style$rules)
							]))
					]));
		case 'NoStaticStyleSheet':
			return $elm$virtual_dom$VirtualDom$text('');
		default:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'elm-ui-static-rules',
				_List_fromArray(
					[
						A2(
						$elm$virtual_dom$VirtualDom$property,
						'rules',
						$elm$json$Json$Encode$string($mdgriffith$elm_ui$Internal$Style$rules))
					]),
				_List_Nil);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 'Serif':
			return 'serif';
		case 'SansSerif':
			return 'sans-serif';
		case 'Monospace':
			return 'monospace';
		case 'Typeface':
			var name = font.a;
			return '\"' + (name + '\"');
		case 'ImportFont':
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.name;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return name === 'smcp';
		case 'VariantOff':
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 'FontWith') {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.variants);
	} else {
		return false;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _v0, existing) {
		var key = _v0.a;
		var val = _v0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var $mdgriffith$elm_ui$Internal$Model$renderStyle = F4(
	function (options, maybePseudo, selector, props) {
		if (maybePseudo.$ === 'Nothing') {
			return _List_fromArray(
				[
					selector + ('{' + (A3(
					$elm$core$List$foldl,
					$mdgriffith$elm_ui$Internal$Model$renderProps(false),
					'',
					props) + '\n}'))
				]);
		} else {
			var pseudo = maybePseudo.a;
			switch (pseudo.$) {
				case 'Hover':
					var _v2 = options.hover;
					switch (_v2.$) {
						case 'NoHover':
							return _List_Nil;
						case 'ForceHover':
							return _List_fromArray(
								[
									selector + ('-hv {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(true),
									'',
									props) + '\n}'))
								]);
						default:
							return _List_fromArray(
								[
									selector + ('-hv:hover {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(false),
									'',
									props) + '\n}'))
								]);
					}
				case 'Focus':
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[
							selector + ('-fs:focus {' + (renderedProps + '\n}')),
							('.' + ($mdgriffith$elm_ui$Internal$Style$classes.any + (':focus ' + (selector + '-fs  {')))) + (renderedProps + '\n}'),
							(selector + '-fs:focus-within {') + (renderedProps + '\n}'),
							('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + (' .focusable-thumb' + (selector + '-fs {')))) + (renderedProps + '\n}')
						]);
				default:
					return _List_fromArray(
						[
							selector + ('-act:active {' + (A3(
							$elm$core$List$foldl,
							$mdgriffith$elm_ui$Internal$Model$renderProps(false),
							'',
							props) + '\n}'))
						]);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return '\"' + (name + '\"');
		case 'VariantOff':
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 'FontWith') {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.variants)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 'Untransformed':
			return $elm$core$Maybe$Nothing;
		case 'Moved':
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'translate3d(' + ($elm$core$String$fromFloat(x) + ('px, ' + ($elm$core$String$fromFloat(y) + ('px, ' + ($elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + ($elm$core$String$fromFloat(tx) + ('px, ' + ($elm$core$String$fromFloat(ty) + ('px, ' + ($elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + ($elm$core$String$fromFloat(sx) + (', ' + ($elm$core$String$fromFloat(sy) + (', ' + ($elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + ($elm$core$String$fromFloat(ox) + (', ' + ($elm$core$String$fromFloat(oy) + (', ' + ($elm$core$String$fromFloat(oz) + (', ' + ($elm$core$String$fromFloat(angle) + 'rad)')))))));
			return $elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderStyleRule = F3(
	function (options, rule, maybePseudo) {
		switch (rule.$) {
			case 'Style':
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 'Shadows':
				var name = rule.a;
				var prop = rule.b;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
						]));
			case 'Transparency':
				var name = rule.a;
				var transparency = rule.b;
				var opacity = A2(
					$elm$core$Basics$max,
					0,
					A2($elm$core$Basics$min, 1, 1 - transparency));
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'opacity',
							$elm$core$String$fromFloat(opacity))
						]));
			case 'FontSize':
				var i = rule.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			case 'FontFamily':
				var name = rule.a;
				var typefaces = rule.b;
				var features = A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
				var families = _List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-family',
						A2(
							$elm$core$String$join,
							', ',
							A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-variant',
						A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
					]);
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, '.' + name, families);
			case 'Single':
				var _class = rule.a;
				var prop = rule.b;
				var val = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, prop, val)
						]));
			case 'Colored':
				var _class = rule.a;
				var prop = rule.b;
				var color = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							prop,
							$mdgriffith$elm_ui$Internal$Model$formatColor(color))
						]));
			case 'SpacingStyle':
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.single;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.row;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.wrapped + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.alignRight;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.paragraph;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.page;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.alignLeft;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.column;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.any;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (row + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (wrappedRow + (' > ' + any)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (column + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_Utils_ap(_class, paragraph),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							'textarea' + (any + _class),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)')),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'height',
									'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::after'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-top',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::before'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-bottom',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								]))
						]));
			case 'PaddingStyle':
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'padding',
							$elm$core$String$fromFloat(top) + ('px ' + ($elm$core$String$fromFloat(right) + ('px ' + ($elm$core$String$fromFloat(bottom) + ('px ' + ($elm$core$String$fromFloat(left) + 'px')))))))
						]));
			case 'BorderWidth':
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'border-width',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 'GridTemplateStyle':
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 'Px':
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 'Content':
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 'Nothing') {
										if (_v2.b.$ === 'Nothing') {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 'Nothing') {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 'Fill':
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 'Nothing') {
										if (_v7.b.$ === 'Nothing') {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 'Nothing') {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 'Min':
									var m = x.a;
									var len = x.b;
									var $temp$minimum = $elm$core$Maybe$Just(m),
										$temp$maximum = maximum,
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
								default:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = minimum,
										$temp$maximum = $elm$core$Maybe$Just(m),
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
							}
						}
					});
				var toGridLength = function (x) {
					return A3(toGridLengthHelper, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, x);
				};
				var xSpacing = toGridLength(template.spacing.a);
				var ySpacing = toGridLength(template.spacing.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.rows)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.columns)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.columns)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.spacing.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.spacing.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.columns)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.rows)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.columns)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 'GridPosition':
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.row) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.height) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.col) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.width) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.row) + (' / ' + ($elm$core$String$fromInt(position.row + position.height) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.col) + (' / ' + ($elm$core$String$fromInt(position.col + position.width) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.row) + ('-' + ($elm$core$String$fromInt(position.col) + ('-' + ($elm$core$String$fromInt(position.width) + ('-' + $elm$core$String$fromInt(position.height)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 'PseudoSelector':
				var _class = rule.a;
				var styles = rule.b;
				var renderPseudoRule = function (style) {
					return A3(
						$mdgriffith$elm_ui$Internal$Model$renderStyleRule,
						options,
						style,
						$elm$core$Maybe$Just(_class));
				};
				return A2($elm$core$List$concatMap, renderPseudoRule, styles);
			default:
				var transform = rule.a;
				var val = $mdgriffith$elm_ui$Internal$Model$transformValue(transform);
				var _class = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				var _v12 = _Utils_Tuple2(_class, val);
				if ((_v12.a.$ === 'Just') && (_v12.b.$ === 'Just')) {
					var cls = _v12.a.a;
					var v = _v12.b.a;
					return A4(
						$mdgriffith$elm_ui$Internal$Model$renderStyle,
						options,
						maybePseudo,
						'.' + cls,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
							]));
				} else {
					return _List_Nil;
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$encodeStyles = F2(
	function (options, stylesheet) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$map,
				function (style) {
					var styled = A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing);
					return _Utils_Tuple2(
						$mdgriffith$elm_ui$Internal$Model$getStyleName(style),
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styled));
				},
				stylesheet));
	});
var $mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_v0) {
			var name = _v0.a;
			var val = _v0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, renderPair, rules)) + '}'));
	});
var $mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _v0) {
		var parentAdj = _v0.a;
		var textAdjustment = _v0.b;
		return _List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.text)))))))))), textAdjustment)
			]);
	});
var $mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _v0, otherFontName) {
		var full = _v0.a;
		var capital = _v0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_Utils_ap(
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.fullSize, full)));
	});
var $mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.text)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {height: height / size, size: size, vertical: vertical};
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.capital, adjustment.baseline, adjustment.descender, adjustment.lowercase]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.descender,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.baseline,
		$elm$core$List$minimum(
			A2(
				$elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.capital,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		capital: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		full: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var $mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				$elm$core$String$fromFloat(converted.height)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.vertical) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.size) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 'Nothing') {
					if (face.$ === 'FontWith') {
						var _with = face.a;
						var _v2 = _with.adjustment;
						if (_v2.$ === 'Nothing') {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.full;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.capital;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		$elm$core$Maybe$Nothing,
		typefaces);
};
var $mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 'ImportFont') {
			var url = font.b;
			return $elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_v2) {
		var name = _v2.a;
		var typefaces = _v2.b;
		var imports = A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2($elm$core$List$map, $elm$core$Tuple$first, rules);
	var fontAdjustments = function (_v1) {
		var name = _v1.a;
		var typefaces = _v1.b;
		var _v0 = $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _v0.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					A2($mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontImports, rules)),
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontAdjustments, rules)));
};
var $mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 'FontFamily') {
		var name = rule.a;
		var typefaces = rule.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var combine = F2(
			function (style, rendered) {
				return {
					rules: _Utils_ap(
						rendered.rules,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					topLevel: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 'Nothing') {
							return rendered.topLevel;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.topLevel);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{rules: _List_Nil, topLevel: _List_Nil},
			stylesheet);
		var topLevel = _v0.topLevel;
		var rules = _v0.rules;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.mode;
		switch (_v0.$) {
			case 'Layout':
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			case 'NoStaticStyleSheet':
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			default:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'elm-ui-rules',
					_List_fromArray(
						[
							A2(
							$elm$virtual_dom$VirtualDom$property,
							'rules',
							A2($mdgriffith$elm_ui$Internal$Model$encodeStyles, options, styleSheet))
						]),
					_List_Nil);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'static-stylesheet',
				$mdgriffith$elm_ui$Internal$Model$staticRoot(opts)),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
				children)) : A2(
			$elm$core$List$cons,
			_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
			children);
	});
var $mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			$mdgriffith$elm_ui$Internal$Model$staticRoot(opts),
			A2($elm$core$List$cons, dynamicStyleSheet, children)) : A2($elm$core$List$cons, dynamicStyleSheet, children);
	});
var $mdgriffith$elm_ui$Internal$Flag$heightBetween = $mdgriffith$elm_ui$Internal$Flag$flag(45);
var $mdgriffith$elm_ui$Internal$Flag$heightFill = $mdgriffith$elm_ui$Internal$Flag$flag(37);
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$core$Bitwise$and = _Bitwise_and;
var $mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _v0) {
		var fieldOne = _v0.a;
		var fieldTwo = _v0.b;
		if (myFlag.$ === 'Flag') {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var $elm$html$Html$s = _VirtualDom_node('s');
var $elm$html$Html$u = _VirtualDom_node('u');
var $mdgriffith$elm_ui$Internal$Flag$widthBetween = $mdgriffith$elm_ui$Internal$Flag$flag(44);
var $mdgriffith$elm_ui$Internal$Flag$widthFill = $mdgriffith$elm_ui$Internal$Flag$flag(39);
var $mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 'Keyed') {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 'NoStyleSheet':
									return keyed;
								case 'OnlyDynamic':
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return $elm$html$Html$div;
								case 'p':
									return $elm$html$Html$p;
								default:
									return $elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 'NoStyleSheet':
									return unkeyed;
								case 'OnlyDynamic':
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 'Generic':
					return A2(createNode, 'div', attributes);
				case 'NodeName':
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						$elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.single))
									]))
							]));
			}
		}();
		switch (parentContext.$) {
			case 'AsRow':
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.contentCenterY, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.contentCenterY, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX])))
						]),
					_List_fromArray(
						[html])) : html));
			case 'AsColumn':
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.widthContent + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.heightContent)))));
var $mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.widthFill + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.heightFill)))));
var $mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementFillClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_v8, _v9) {
				var key = _v8.a;
				var child = _v8.b;
				var htmls = _v9.a;
				var existingStyles = _v9.b;
				switch (child.$) {
					case 'Unstyled':
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 'Styled':
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles));
					case 'Text':
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _v6) {
				var htmls = _v6.a;
				var existingStyles = _v6.b;
				switch (child.$) {
					case 'Unstyled':
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 'Styled':
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles));
					case 'Text':
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 'Keyed') {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.styles : _Utils_ap(rendered.styles, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.has,
						rendered.node,
						rendered.attributes,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.children)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.has,
							rendered.node,
							rendered.attributes,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.children))),
						styles: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _v3 = A3(
				$elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _v3.a;
			var styles = _v3.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.styles : _Utils_ap(rendered.styles, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.has,
						rendered.node,
						rendered.attributes,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.children)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.has,
							rendered.node,
							rendered.attributes,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.children))),
						styles: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 'Single', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 'Transform', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (myFlag.$ === 'Flag') {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 'ChildrenBehind', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 'ChildrenBehindAndInFront', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 'ChildrenInFront', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location.$) {
							case 'Above':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.above]));
							case 'Below':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.below]));
							case 'OnRight':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.onRight]));
							case 'OnLeft':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.onLeft]));
							case 'InFront':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.inFront]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.behind]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 'Empty':
							return $elm$virtual_dom$VirtualDom$text('');
						case 'Text':
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 'Unstyled':
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 'NoNearbyChildren':
				if (location.$ === 'Behind') {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 'ChildrenBehind':
				var existingBehind = existing.a;
				if (location.$ === 'Behind') {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 'ChildrenInFront':
				var existingInFront = existing.a;
				if (location.$ === 'Behind') {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2($elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location.$ === 'Behind') {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2($elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2($elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 'Embedded', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 'NodeName', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 'Generic':
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 'NodeName':
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align.$) {
		case 'Left':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignLeft);
		case 'Right':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignRight);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignCenterX);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align.$) {
		case 'Top':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignTop);
		case 'Bottom':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignBottom);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignCenterY);
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 'FullTransform', a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 'Moved', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 'Untransformed':
				switch (component.$) {
					case 'MoveX':
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 'MoveY':
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 'MoveZ':
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 'MoveXYZ':
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 'Rotate':
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 'Moved':
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 'MoveX':
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 'MoveY':
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 'MoveZ':
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 'MoveXYZ':
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 'Rotate':
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 'MoveX':
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 'MoveY':
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 'MoveZ':
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 'MoveXYZ':
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 'Rotate':
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$height = $mdgriffith$elm_ui$Internal$Flag$flag(7);
var $mdgriffith$elm_ui$Internal$Flag$heightContent = $mdgriffith$elm_ui$Internal$Flag$flag(36);
var $mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_v0, _v1) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v1.a;
		var four = _v1.b;
		return A2($mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var $mdgriffith$elm_ui$Internal$Flag$none = A2($mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 'Px':
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.heightExact + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 'Content':
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.heightContent,
				_List_Nil);
		case 'Fill':
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.heightFill,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.column + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 'Min':
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				$elm$core$String$fromInt(minSize) + 'px !important');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$widthContent = $mdgriffith$elm_ui$Internal$Flag$flag(38);
var $mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 'Px':
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.widthExact + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 'Content':
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.widthContent,
				_List_Nil);
		case 'Fill':
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.widthFill,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 'Min':
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$borderWidth = $mdgriffith$elm_ui$Internal$Flag$flag(27);
var $mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, $mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 'Single') {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 'FontSize':
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 'PaddingStyle':
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$width = $mdgriffith$elm_ui$Internal$Flag$flag(6);
var $mdgriffith$elm_ui$Internal$Flag$xAlign = $mdgriffith$elm_ui$Internal$Flag$flag(30);
var $mdgriffith$elm_ui$Internal$Flag$yAlign = $mdgriffith$elm_ui$Internal$Flag$flag(29);
var $mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _v1 = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_v1.$ === 'Nothing') {
					return {
						attributes: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						children: children,
						has: has,
						node: node,
						styles: styles
					};
				} else {
					var _class = _v1.a;
					return {
						attributes: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						children: children,
						has: has,
						node: node,
						styles: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 'NoAttribute':
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'Class':
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 'Attr':
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2($elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'StyleClass':
						var flag = attribute.a;
						var style = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2($mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2($elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 'TransformComponent':
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'Width':
						var width = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 'Px':
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.widthExact + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3(
											$mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + $elm$core$String$fromInt(px),
											'width',
											$elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Content':
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.widthContent),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Fill':
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.widthFill),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion + (' width-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v4 = $mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _v4.a;
									var newClass = _v4.b;
									var newStyles = _v4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 'Height':
						var height = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 'Px':
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.heightExact + (' ' + (name + (' ' + classes))),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Content':
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.heightContent + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Fill':
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.heightFill + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion + (' height-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.column + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v6 = $mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _v6.a;
									var newClass = _v6.b;
									var newStyles = _v6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 'Describe':
						var description = attribute.a;
						switch (description.$) {
							case 'Main':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Navigation':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'ContentInfo':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Complementary':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Heading':
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											$mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + $elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 'Paragraph':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Button':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Label':
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'LivePolite':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 'Nearby':
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 'Empty':
									return styles;
								case 'Text':
									var str = elem.a;
									return styles;
								case 'Unstyled':
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.styles);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3($mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'AlignX':
						var x = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x.$) {
									case 'CenterX':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 'Right':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y.$) {
									case 'CenterY':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 'Bottom':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 'Untransformed'};
var $mdgriffith$elm_ui$Internal$Model$untransformed = $mdgriffith$elm_ui$Internal$Model$Untransformed;
var $mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			$mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				$mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				$mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				$mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				$elm$core$List$reverse(attributes)));
	});
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 'Height', a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 'Content'};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 'Width', a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					attrs)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var $mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 'Fill', a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $author$project$Common$Urls$frontendWriteLetterUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['write-letter']),
	_List_Nil);
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 'Attr', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 'OnlyDynamic', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 'StaticRootAndDynamic', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$AllowHover = {$: 'AllowHover'};
var $mdgriffith$elm_ui$Internal$Model$Layout = {$: 'Layout'};
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	backgroundColor: $elm$core$Maybe$Nothing,
	borderColor: $elm$core$Maybe$Nothing,
	shadow: $elm$core$Maybe$Just(
		{
			blur: 0,
			color: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			offset: _Utils_Tuple2(0, 0),
			size: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 'HoverOption':
					var hoverable = opt.a;
					var _v4 = record.hover;
					if (_v4.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								hover: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 'FocusStyleOption':
					var focusStyle = opt.a;
					var _v5 = record.focus;
					if (_v5.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								focus: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.mode;
					if (_v6.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								mode: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			focus: function () {
				var _v0 = record.focus;
				if (_v0.$ === 'Nothing') {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			hover: function () {
				var _v1 = record.hover;
				if (_v1.$ === 'Nothing') {
					return $mdgriffith$elm_ui$Internal$Model$AllowHover;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			mode: function () {
				var _v2 = record.mode;
				if (_v2.$ === 'Nothing') {
					return $mdgriffith$elm_ui$Internal$Model$Layout;
				} else {
					var actualMode = _v2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			$elm$core$List$foldr,
			combine,
			{focus: $elm$core$Maybe$Nothing, hover: $elm$core$Maybe$Nothing, mode: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 'Unstyled':
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 'Styled':
				var styles = el.a.styles;
				var html = el.a.html;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 'Text':
				var text = el.a;
				return $mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return $mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = $mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _v0 = options.mode;
			if (_v0.$ === 'NoStaticStyleSheet') {
				return $mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var $mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 'FontFamily', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 'FontSize', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 'SansSerif'};
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 'Typeface', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$fontFamily = $mdgriffith$elm_ui$Internal$Flag$flag(5);
var $mdgriffith$elm_ui$Internal$Flag$fontSize = $mdgriffith$elm_ui$Internal$Flag$flag(4);
var $elm$core$String$toLower = _String_toLower;
var $elm$core$String$words = _String_words;
var $mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 'Serif':
						return 'serif';
					case 'SansSerif':
						return 'sans-serif';
					case 'Monospace':
						return 'monospace';
					case 'Typeface':
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 'ImportFont':
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.name;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
				}
			}());
	});
var $mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			$mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			$mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontSize,
			$mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				$mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var $mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_v0, attrs, child) {
		var options = _v0.options;
		return A3(
			$mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[$mdgriffith$elm_ui$Internal$Style$classes.root, $mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{options: _List_Nil});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $mdgriffith$elm_ui$Element$link = F2(
	function (attrs, _v0) {
		var url = _v0.url;
		var label = _v0.label;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$NodeName('a'),
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Attr(
					$elm$html$Html$Attributes$href(url)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Internal$Model$Attr(
						$elm$html$Html$Attributes$rel('noopener noreferrer')),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentCenterX + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.contentCenterY + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.link)))),
								attrs))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 'PaddingStyle', a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
var $mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		if (_Utils_eq(x, y)) {
			var f = x;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$padding,
				A5(
					$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
					'p-' + $elm$core$String$fromInt(x),
					f,
					f,
					f,
					f));
		} else {
			var yFloat = y;
			var xFloat = x;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$padding,
				A5(
					$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
					'p-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
					yFloat,
					xFloat,
					yFloat,
					xFloat));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 'Describe', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Paragraph = {$: 'Paragraph'};
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 'SpacingStyle', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Flag$spacing = $mdgriffith$elm_ui$Internal$Flag$flag(3);
var $mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y)));
	});
var $mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var $mdgriffith$elm_ui$Element$paragraph = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asParagraph,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Paragraph),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$spacing(5),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 'Text', a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $author$project$Common$Contents$plainPara = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$paragraph,
		_List_Nil,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$text(str)
			]));
};
var $mdgriffith$elm_ui$Element$spacingXY = F2(
	function (x, y) {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$spacing,
			A3(
				$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
				A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, y),
				x,
				y));
	});
var $mdgriffith$elm_ui$Internal$Model$AsTextColumn = {$: 'AsTextColumn'};
var $mdgriffith$elm_ui$Internal$Model$asTextColumn = $mdgriffith$elm_ui$Internal$Model$AsTextColumn;
var $mdgriffith$elm_ui$Internal$Model$Max = F2(
	function (a, b) {
		return {$: 'Max', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$maximum = F2(
	function (i, l) {
		return A2($mdgriffith$elm_ui$Internal$Model$Max, i, l);
	});
var $mdgriffith$elm_ui$Internal$Model$Min = F2(
	function (a, b) {
		return {$: 'Min', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$minimum = F2(
	function (i, l) {
		return A2($mdgriffith$elm_ui$Internal$Model$Min, i, l);
	});
var $mdgriffith$elm_ui$Element$textColumn = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asTextColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width(
					A2(
						$mdgriffith$elm_ui$Element$maximum,
						750,
						A2($mdgriffith$elm_ui$Element$minimum, 500, $mdgriffith$elm_ui$Element$fill))),
				attrs),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $author$project$CoreTypes$MessageInput = function (a) {
	return {$: 'MessageInput', a: a};
};
var $author$project$CoreTypes$MessageSend = {$: 'MessageSend'};
var $author$project$CoreTypes$NameChange = {$: 'NameChange'};
var $author$project$CoreTypes$NewNameInput = function (a) {
	return {$: 'NewNameInput', a: a};
};
var $author$project$CoreTypes$OnChatInputFocal = function (a) {
	return {$: 'OnChatInputFocal', a: a};
};
var $author$project$Chat$OnManualScrolled = {$: 'OnManualScrolled'};
var $author$project$Chat$OnNewMsgHintClicked = {$: 'OnNewMsgHintClicked'};
var $mdgriffith$elm_ui$Internal$Model$Button = {$: 'Button'};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var $mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 'NoAttribute'};
var $mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 'StyleClass') && (attr.b.$ === 'PseudoSelector')) && (attr.b.a.$ === 'Focus')) {
		var _v1 = attr.b;
		var _v2 = _v1.a;
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $mdgriffith$elm_ui$Element$Events$onClick = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Events$onClick);
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $mdgriffith$elm_ui$Element$Input$onKeyLookup = function (lookup) {
	var decode = function (code) {
		var _v0 = lookup(code);
		if (_v0.$ === 'Nothing') {
			return $elm$json$Json$Decode$fail('No key matched');
		} else {
			var msg = _v0.a;
			return $elm$json$Json$Decode$succeed(msg);
		}
	};
	var isKey = A2(
		$elm$json$Json$Decode$andThen,
		decode,
		A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		A2(
			$elm$html$Html$Events$preventDefaultOn,
			'keydown',
			A2(
				$elm$json$Json$Decode$map,
				function (fired) {
					return _Utils_Tuple2(fired, true);
				},
				isKey)));
};
var $mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 'Class', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$cursor = $mdgriffith$elm_ui$Internal$Flag$flag(21);
var $mdgriffith$elm_ui$Element$pointer = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cursorPointer);
var $mdgriffith$elm_ui$Element$Input$space = ' ';
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _v0) {
		var onPress = _v0.onPress;
		var label = _v0.label;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentCenterX + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.contentCenterY + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.seButton + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.noTextSelection)))))),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$pointer,
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Internal$Model$Attr(
											$elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 'Nothing') {
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Internal$Model$Attr(
														$elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														$elm$core$List$cons,
														$mdgriffith$elm_ui$Element$Input$onKeyLookup(
															function (code) {
																return _Utils_eq(code, $mdgriffith$elm_ui$Element$Input$enter) ? $elm$core$Maybe$Just(msg) : (_Utils_eq(code, $mdgriffith$elm_ui$Element$Input$space) ? $elm$core$Maybe$Just(msg) : $elm$core$Maybe$Nothing);
															}),
														attrs));
											}
										}()))))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 'AlignX', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterX = {$: 'CenterX'};
var $mdgriffith$elm_ui$Element$centerX = $mdgriffith$elm_ui$Internal$Model$AlignX($mdgriffith$elm_ui$Internal$Model$CenterX);
var $mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 'AlignY', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterY = {$: 'CenterY'};
var $mdgriffith$elm_ui$Element$centerY = $mdgriffith$elm_ui$Internal$Model$AlignY($mdgriffith$elm_ui$Internal$Model$CenterY);
var $author$project$Views$Chat$sideColumnGap = 60;
var $author$project$Views$Chat$sideColumnWidthPx = 400;
var $author$project$Common$Styles$windowPaddingPx = function (windowWidth) {
	return (60 / 2560) * windowWidth;
};
var $author$project$Views$Chat$chatColumnMaxWidthPx = function (windowWidth) {
	return $elm$core$Basics$round(
		((windowWidth - ($author$project$Common$Styles$windowPaddingPx(windowWidth) * 2)) - $author$project$Views$Chat$sideColumnWidthPx) - $author$project$Views$Chat$sideColumnGap);
};
var $mdgriffith$elm_ui$Internal$Model$AsColumn = {$: 'AsColumn'};
var $mdgriffith$elm_ui$Internal$Model$asColumn = $mdgriffith$elm_ui$Internal$Model$AsColumn;
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentTop + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.contentLeft)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Element$fillPortion = $mdgriffith$elm_ui$Internal$Model$Fill;
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $mdgriffith$elm_ui$Element$Input$Above = {$: 'Above'};
var $mdgriffith$elm_ui$Element$Input$Label = F3(
	function (a, b, c) {
		return {$: 'Label', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Element$Input$labelAbove = $mdgriffith$elm_ui$Element$Input$Label($mdgriffith$elm_ui$Element$Input$Above);
var $mdgriffith$elm_ui$Element$Input$OnLeft = {$: 'OnLeft'};
var $mdgriffith$elm_ui$Element$Input$labelLeft = $mdgriffith$elm_ui$Element$Input$Label($mdgriffith$elm_ui$Element$Input$OnLeft);
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Chat$isMetaMsg = function (msg) {
	var _v0 = msg.msgFromClient.msgType;
	if (_v0.$ === 'Content') {
		return false;
	} else {
		return true;
	}
};
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $author$project$Utils$Utils$posixSecToPosix = function (posixSec) {
	var posixMilliSec = posixSec * 1000;
	return $elm$time$Time$millisToPosix(posixMilliSec);
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Chat$mkMsgBundles = function (chatMsgMetas) {
	var combine = F2(
		function (msg, bundles) {
			var appended = A2(
				$elm$core$List$cons,
				{
					msgs: _List_fromArray(
						[msg]),
					time: $author$project$Utils$Utils$posixSecToPosix(msg.posixTimeSec),
					userId: msg.userId,
					username: msg.username
				},
				bundles);
			var _v0 = A3(
				$elm$core$Maybe$map2,
				$elm$core$Tuple$pair,
				$elm$core$List$head(bundles),
				$elm$core$List$tail(bundles));
			if (_v0.$ === 'Just') {
				var _v1 = _v0.a;
				var headBundle = _v1.a;
				var tailBundles = _v1.b;
				if ($author$project$Chat$isMetaMsg(msg) || A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$author$project$Chat$isMetaMsg,
						$elm$core$List$head(headBundle.msgs)))) {
					return appended;
				} else {
					if (_Utils_eq(msg.userId, headBundle.userId)) {
						var updatedHeadBundle = _Utils_update(
							headBundle,
							{
								msgs: A2($elm$core$List$cons, msg, headBundle.msgs)
							});
						return A2($elm$core$List$cons, updatedHeadBundle, tailBundles);
					} else {
						return appended;
					}
				}
			} else {
				return appended;
			}
		});
	return A3($elm$core$List$foldr, combine, _List_Nil, chatMsgMetas);
};
var $mdgriffith$elm_ui$Internal$Flag$fontWeight = $mdgriffith$elm_ui$Internal$Flag$flag(13);
var $mdgriffith$elm_ui$Element$Font$bold = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.bold);
var $author$project$Chat$isMetaBundle = function (bundle) {
	var _v0 = A2(
		$elm$core$Maybe$map,
		$author$project$Chat$isMetaMsg,
		$elm$core$List$head(bundle.msgs));
	if ((_v0.$ === 'Just') && _v0.a) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Common$Colors$green = A3($mdgriffith$elm_ui$Element$rgb255, 100, 200, 0);
var $mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left)))))));
	});
var $mdgriffith$elm_ui$Element$paddingEach = function (_v0) {
	var top = _v0.top;
	var right = _v0.right;
	var bottom = _v0.bottom;
	var left = _v0.left;
	if (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) {
		var topFloat = top;
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + $elm$core$String$fromInt(top),
				topFloat,
				topFloat,
				topFloat,
				topFloat));
	} else {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				A4($mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
				top,
				right,
				bottom,
				left));
	}
};
var $elm_community$string_extra$String$Extra$surround = F2(
	function (wrapper, string) {
		return _Utils_ap(
			wrapper,
			_Utils_ap(string, wrapper));
	});
var $elm_community$string_extra$String$Extra$quote = function (string) {
	return A2($elm_community$string_extra$String$Extra$surround, '\"', string);
};
var $author$project$Common$Colors$red = A3($mdgriffith$elm_ui$Element$rgb255, 230, 6, 6);
var $dillonkearns$elm_markdown$Markdown$Parser$problemToString = function (problem) {
	switch (problem.$) {
		case 'Expecting':
			var string = problem.a;
			return 'Expecting ' + string;
		case 'ExpectingInt':
			return 'Expecting int';
		case 'ExpectingHex':
			return 'Expecting hex';
		case 'ExpectingOctal':
			return 'Expecting octal';
		case 'ExpectingBinary':
			return 'Expecting binary';
		case 'ExpectingFloat':
			return 'Expecting float';
		case 'ExpectingNumber':
			return 'Expecting number';
		case 'ExpectingVariable':
			return 'Expecting variable';
		case 'ExpectingSymbol':
			var string = problem.a;
			return 'Expecting symbol ' + string;
		case 'ExpectingKeyword':
			var string = problem.a;
			return 'Expecting keyword ' + string;
		case 'ExpectingEnd':
			return 'Expecting keyword end';
		case 'UnexpectedChar':
			return 'Unexpected char';
		case 'Problem':
			var problemDescription = problem.a;
			return problemDescription;
		default:
			return 'Bad repeat';
	}
};
var $dillonkearns$elm_markdown$Markdown$Parser$deadEndToString = function (deadEnd) {
	return 'Problem at row ' + ($elm$core$String$fromInt(deadEnd.row) + ('\n' + $dillonkearns$elm_markdown$Markdown$Parser$problemToString(deadEnd.problem)));
};
var $dillonkearns$elm_markdown$Markdown$Block$BlockQuote = function (a) {
	return {$: 'BlockQuote', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Cdata = function (a) {
	return {$: 'Cdata', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$CodeBlock = function (a) {
	return {$: 'CodeBlock', a: a};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$CodeBlock = function (a) {
	return {$: 'CodeBlock', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$CodeSpan = function (a) {
	return {$: 'CodeSpan', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$CompletedTask = {$: 'CompletedTask'};
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Emphasis = function (a) {
	return {$: 'Emphasis', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Parser$EmptyBlock = {$: 'EmptyBlock'};
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HardLineBreak = {$: 'HardLineBreak'};
var $dillonkearns$elm_markdown$Markdown$Block$Heading = F2(
	function (a, b) {
		return {$: 'Heading', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$RawBlock$Html = function (a) {
	return {$: 'Html', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HtmlBlock = function (a) {
	return {$: 'HtmlBlock', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HtmlComment = function (a) {
	return {$: 'HtmlComment', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HtmlDeclaration = F2(
	function (a, b) {
		return {$: 'HtmlDeclaration', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Block$HtmlElement = F3(
	function (a, b, c) {
		return {$: 'HtmlElement', a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Block$HtmlInline = function (a) {
	return {$: 'HtmlInline', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Image = F3(
	function (a, b, c) {
		return {$: 'Image', a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Block$IncompleteTask = {$: 'IncompleteTask'};
var $dillonkearns$elm_markdown$Markdown$Parser$InlineProblem = function (a) {
	return {$: 'InlineProblem', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Link = F3(
	function (a, b, c) {
		return {$: 'Link', a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Block$ListItem = F2(
	function (a, b) {
		return {$: 'ListItem', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$NoTask = {$: 'NoTask'};
var $dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph = function (a) {
	return {$: 'OpenBlockOrParagraph', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$OrderedList = F2(
	function (a, b) {
		return {$: 'OrderedList', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Block$Paragraph = function (a) {
	return {$: 'Paragraph', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock = function (a) {
	return {$: 'ParsedBlock', a: a};
};
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$ProcessingInstruction = function (a) {
	return {$: 'ProcessingInstruction', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Strikethrough = function (a) {
	return {$: 'Strikethrough', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Strong = function (a) {
	return {$: 'Strong', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Table = F2(
	function (a, b) {
		return {$: 'Table', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Block$Text = function (a) {
	return {$: 'Text', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$ThematicBreak = {$: 'ThematicBreak'};
var $dillonkearns$elm_markdown$Markdown$RawBlock$ThematicBreak = {$: 'ThematicBreak'};
var $dillonkearns$elm_markdown$Markdown$Block$UnorderedList = function (a) {
	return {$: 'UnorderedList', a: a};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines = function (a) {
	return {$: 'UnparsedInlines', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Parser$addReference = F2(
	function (state, linkRef) {
		return {
			linkReferenceDefinitions: A2($elm$core$List$cons, linkRef, state.linkReferenceDefinitions),
			rawBlocks: state.rawBlocks
		};
	});
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0.a;
	return $elm$parser$Parser$Advanced$Parser(
		function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 'Bad') {
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, false, x);
			} else {
				var a = _v1.b;
				var s1 = _v1.c;
				return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
			}
		});
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$BlankLine = {$: 'BlankLine'};
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $dillonkearns$elm_markdown$Whitespace$isSpaceOrTab = function (_char) {
	switch (_char.valueOf()) {
		case ' ':
			return true;
		case '\t':
			return true;
		default:
			return false;
	}
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Parser$Token$carriageReturn = A2(
	$elm$parser$Parser$Advanced$Token,
	'\r',
	$elm$parser$Parser$Expecting('a carriage return'));
var $dillonkearns$elm_markdown$Parser$Token$newline = A2(
	$elm$parser$Parser$Advanced$Token,
	'\n',
	$elm$parser$Parser$Expecting('a newline'));
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $dillonkearns$elm_markdown$Whitespace$lineEnd = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			$elm$parser$Parser$Advanced$token($dillonkearns$elm_markdown$Parser$Token$newline),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$token($dillonkearns$elm_markdown$Parser$Token$carriageReturn),
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						$elm$parser$Parser$Advanced$token($dillonkearns$elm_markdown$Parser$Token$newline),
						$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
					])))
		]));
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $dillonkearns$elm_markdown$Markdown$Parser$blankLine = A2(
	$elm$parser$Parser$Advanced$map,
	function (_v0) {
		return $dillonkearns$elm_markdown$Markdown$RawBlock$BlankLine;
	},
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$backtrackable(
			$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab)),
		$dillonkearns$elm_markdown$Whitespace$lineEnd));
var $dillonkearns$elm_markdown$Markdown$RawBlock$BlockQuote = function (a) {
	return {$: 'BlockQuote', a: a};
};
var $dillonkearns$elm_markdown$Parser$Token$space = A2(
	$elm$parser$Parser$Advanced$Token,
	' ',
	$elm$parser$Parser$Expecting('a space'));
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $dillonkearns$elm_markdown$Markdown$Parser$blockQuoteStarts = _List_fromArray(
	[
		$elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			'>',
			$elm$parser$Parser$Expecting('>'))),
		A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$backtrackable(
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$space)),
		$elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$Advanced$symbol(
					A2(
						$elm$parser$Parser$Advanced$Token,
						'>',
						$elm$parser$Parser$Expecting(' >'))),
					$elm$parser$Parser$Advanced$symbol(
					A2(
						$elm$parser$Parser$Advanced$Token,
						' >',
						$elm$parser$Parser$Expecting('  >'))),
					$elm$parser$Parser$Advanced$symbol(
					A2(
						$elm$parser$Parser$Advanced$Token,
						'  >',
						$elm$parser$Parser$Expecting('   >')))
				])))
	]);
var $dillonkearns$elm_markdown$Whitespace$isLineEnd = function (_char) {
	switch (_char.valueOf()) {
		case '\n':
			return true;
		case '\r':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd = $elm$parser$Parser$Advanced$chompWhile(
	A2($elm$core$Basics$composeL, $elm$core$Basics$not, $dillonkearns$elm_markdown$Whitespace$isLineEnd));
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $dillonkearns$elm_markdown$Helpers$endOfFile = $elm$parser$Parser$Advanced$end(
	$elm$parser$Parser$Expecting('the end of the input'));
var $dillonkearns$elm_markdown$Helpers$lineEndOrEnd = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[$dillonkearns$elm_markdown$Whitespace$lineEnd, $dillonkearns$elm_markdown$Helpers$endOfFile]));
var $dillonkearns$elm_markdown$Markdown$Parser$blockQuote = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$RawBlock$BlockQuote),
			$elm$parser$Parser$Advanced$oneOf($dillonkearns$elm_markdown$Markdown$Parser$blockQuoteStarts)),
		$elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$space),
					$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
				]))),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString($dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
		$dillonkearns$elm_markdown$Helpers$lineEndOrEnd));
var $dillonkearns$elm_markdown$Markdown$RawBlock$Heading = F2(
	function (a, b) {
		return {$: 'Heading', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$RawBlock$IndentedCodeBlock = function (a) {
	return {$: 'IndentedCodeBlock', a: a};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$Table = function (a) {
	return {$: 'Table', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Table$Table = F2(
	function (a, b) {
		return {$: 'Table', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Table$TableDelimiterRow = F2(
	function (a, b) {
		return {$: 'TableDelimiterRow', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith = F3(
	function (joinWith, string1, string2) {
		var _v0 = _Utils_Tuple2(string1, string2);
		if (_v0.a === '') {
			return string2;
		} else {
			if (_v0.b === '') {
				return string1;
			} else {
				return _Utils_ap(
					string1,
					_Utils_ap(joinWith, string2));
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$joinStringsPreserveAll = F2(
	function (string1, string2) {
		return string1 + ('\n' + string2);
	});
var $dillonkearns$elm_markdown$Markdown$Table$TableHeader = function (a) {
	return {$: 'TableHeader', a: a};
};
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0.a;
			var _v1 = parse(s0);
			if (_v1.$ === 'Good') {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var $dillonkearns$elm_markdown$Parser$Token$parseString = function (str) {
	return $elm$parser$Parser$Advanced$token(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$Expecting(str)));
};
var $dillonkearns$elm_markdown$Markdown$TableParser$parseCellHelper = function (_v0) {
	var curr = _v0.a;
	var acc = _v0.b;
	var _return = A2(
		$elm$core$Maybe$withDefault,
		$elm$parser$Parser$Advanced$Done(acc),
		A2(
			$elm$core$Maybe$map,
			function (cell) {
				return $elm$parser$Parser$Advanced$Done(
					A2($elm$core$List$cons, cell, acc));
			},
			curr));
	var finishCell = A2(
		$elm$core$Maybe$withDefault,
		$elm$parser$Parser$Advanced$Loop(
			_Utils_Tuple2($elm$core$Maybe$Nothing, acc)),
		A2(
			$elm$core$Maybe$map,
			function (cell) {
				return $elm$parser$Parser$Advanced$Loop(
					_Utils_Tuple2(
						$elm$core$Maybe$Nothing,
						A2($elm$core$List$cons, cell, acc)));
			},
			curr));
	var addToCurrent = function (c) {
		return _Utils_ap(
			A2($elm$core$Maybe$withDefault, '', curr),
			c);
	};
	var continueCell = function (c) {
		return $elm$parser$Parser$Advanced$Loop(
			_Utils_Tuple2(
				$elm$core$Maybe$Just(
					addToCurrent(c)),
				acc));
	};
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v1) {
					return _return;
				},
				$dillonkearns$elm_markdown$Parser$Token$parseString('|\n')),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v2) {
					return _return;
				},
				$dillonkearns$elm_markdown$Parser$Token$parseString('\n')),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v3) {
					return _return;
				},
				$elm$parser$Parser$Advanced$end(
					$elm$parser$Parser$Expecting('end'))),
				A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$backtrackable(
					$elm$parser$Parser$Advanced$succeed(
						continueCell('|'))),
				$dillonkearns$elm_markdown$Parser$Token$parseString('\\\\|')),
				A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$backtrackable(
					$elm$parser$Parser$Advanced$succeed(
						continueCell('\\'))),
				$dillonkearns$elm_markdown$Parser$Token$parseString('\\\\')),
				A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$backtrackable(
					$elm$parser$Parser$Advanced$succeed(
						continueCell('|'))),
				$dillonkearns$elm_markdown$Parser$Token$parseString('\\|')),
				A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$backtrackable(
					$elm$parser$Parser$Advanced$succeed(finishCell)),
				$dillonkearns$elm_markdown$Parser$Token$parseString('|')),
				A2(
				$elm$parser$Parser$Advanced$mapChompedString,
				F2(
					function (_char, _v4) {
						return continueCell(_char);
					}),
				A2(
					$elm$parser$Parser$Advanced$chompIf,
					$elm$core$Basics$always(true),
					$elm$parser$Parser$Problem('No character found')))
			]));
};
var $elm$core$String$trim = _String_trim;
var $dillonkearns$elm_markdown$Markdown$TableParser$parseCells = A2(
	$elm$parser$Parser$Advanced$map,
	A2(
		$elm$core$List$foldl,
		F2(
			function (cell, acc) {
				return A2(
					$elm$core$List$cons,
					$elm$core$String$trim(cell),
					acc);
			}),
		_List_Nil),
	A2(
		$elm$parser$Parser$Advanced$loop,
		_Utils_Tuple2($elm$core$Maybe$Nothing, _List_Nil),
		$dillonkearns$elm_markdown$Markdown$TableParser$parseCellHelper));
var $dillonkearns$elm_markdown$Markdown$TableParser$rowParser = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					$dillonkearns$elm_markdown$Parser$Token$parseString('|'),
					$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
				]))),
	$dillonkearns$elm_markdown$Markdown$TableParser$parseCells);
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $dillonkearns$elm_markdown$Markdown$TableParser$parseHeader = F2(
	function (_v0, headersRow) {
		var columnAlignments = _v0.b;
		var headersWithAlignment = function (headers) {
			return A3(
				$elm$core$List$map2,
				F2(
					function (headerCell, alignment) {
						return {alignment: alignment, label: headerCell};
					}),
				headers,
				columnAlignments);
		};
		var combineHeaderAndDelimiter = function (headers) {
			return _Utils_eq(
				$elm$core$List$length(headers),
				$elm$core$List$length(columnAlignments)) ? $elm$core$Result$Ok(
				$dillonkearns$elm_markdown$Markdown$Table$TableHeader(
					headersWithAlignment(headers))) : $elm$core$Result$Err(
				'Tables must have the same number of header columns (' + ($elm$core$String$fromInt(
					$elm$core$List$length(headers)) + (') as delimiter columns (' + ($elm$core$String$fromInt(
					$elm$core$List$length(columnAlignments)) + ')'))));
		};
		var _v1 = A2($elm$parser$Parser$Advanced$run, $dillonkearns$elm_markdown$Markdown$TableParser$rowParser, headersRow);
		if (_v1.$ === 'Ok') {
			var headers = _v1.a;
			return combineHeaderAndDelimiter(headers);
		} else {
			return $elm$core$Result$Err('Unable to parse previous line as a table header');
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$completeOrMergeBlocks = F2(
	function (state, newRawBlock) {
		return {
			linkReferenceDefinitions: state.linkReferenceDefinitions,
			rawBlocks: function () {
				var _v0 = _Utils_Tuple2(newRawBlock, state.rawBlocks);
				_v0$9:
				while (true) {
					if (_v0.b.b) {
						switch (_v0.b.a.$) {
							case 'CodeBlock':
								if (_v0.a.$ === 'CodeBlock') {
									var block1 = _v0.a.a;
									var _v1 = _v0.b;
									var block2 = _v1.a.a;
									var rest = _v1.b;
									return A2(
										$elm$core$List$cons,
										$dillonkearns$elm_markdown$Markdown$RawBlock$CodeBlock(
											{
												body: A2($dillonkearns$elm_markdown$Markdown$Parser$joinStringsPreserveAll, block2.body, block1.body),
												language: $elm$core$Maybe$Nothing
											}),
										rest);
								} else {
									break _v0$9;
								}
							case 'IndentedCodeBlock':
								if (_v0.a.$ === 'IndentedCodeBlock') {
									var block1 = _v0.a.a;
									var _v2 = _v0.b;
									var block2 = _v2.a.a;
									var rest = _v2.b;
									return A2(
										$elm$core$List$cons,
										$dillonkearns$elm_markdown$Markdown$RawBlock$IndentedCodeBlock(
											A2($dillonkearns$elm_markdown$Markdown$Parser$joinStringsPreserveAll, block2, block1)),
										rest);
								} else {
									break _v0$9;
								}
							case 'BlockQuote':
								switch (_v0.a.$) {
									case 'OpenBlockOrParagraph':
										var body1 = _v0.a.a.a;
										var _v3 = _v0.b;
										var body2 = _v3.a.a;
										var rest = _v3.b;
										return A2(
											$elm$core$List$cons,
											$dillonkearns$elm_markdown$Markdown$RawBlock$BlockQuote(
												A3($dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith, '\n', body2, body1)),
											rest);
									case 'BlockQuote':
										var body1 = _v0.a.a;
										var _v4 = _v0.b;
										var body2 = _v4.a.a;
										var rest = _v4.b;
										return A2(
											$elm$core$List$cons,
											$dillonkearns$elm_markdown$Markdown$RawBlock$BlockQuote(
												A2($dillonkearns$elm_markdown$Markdown$Parser$joinStringsPreserveAll, body2, body1)),
											rest);
									default:
										break _v0$9;
								}
							case 'OpenBlockOrParagraph':
								switch (_v0.a.$) {
									case 'OpenBlockOrParagraph':
										var body1 = _v0.a.a.a;
										var _v5 = _v0.b;
										var body2 = _v5.a.a.a;
										var rest = _v5.b;
										return A2(
											$elm$core$List$cons,
											$dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
												$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(
													A3($dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith, '\n', body2, body1))),
											rest);
									case 'SetextLine':
										if (_v0.a.a.$ === 'LevelOne') {
											var _v6 = _v0.a;
											var _v7 = _v6.a;
											var _v8 = _v0.b;
											var unparsedInlines = _v8.a.a;
											var rest = _v8.b;
											return A2(
												$elm$core$List$cons,
												A2($dillonkearns$elm_markdown$Markdown$RawBlock$Heading, 1, unparsedInlines),
												rest);
										} else {
											var _v9 = _v0.a;
											var _v10 = _v9.a;
											var _v11 = _v0.b;
											var unparsedInlines = _v11.a.a;
											var rest = _v11.b;
											return A2(
												$elm$core$List$cons,
												A2($dillonkearns$elm_markdown$Markdown$RawBlock$Heading, 2, unparsedInlines),
												rest);
										}
									case 'TableDelimiter':
										var _v12 = _v0.a.a;
										var text = _v12.a;
										var alignments = _v12.b;
										var _v13 = _v0.b;
										var rawHeaders = _v13.a.a.a;
										var rest = _v13.b;
										var _v14 = A2(
											$dillonkearns$elm_markdown$Markdown$TableParser$parseHeader,
											A2($dillonkearns$elm_markdown$Markdown$Table$TableDelimiterRow, text, alignments),
											rawHeaders);
										if (_v14.$ === 'Ok') {
											var headers = _v14.a.a;
											return A2(
												$elm$core$List$cons,
												$dillonkearns$elm_markdown$Markdown$RawBlock$Table(
													A2($dillonkearns$elm_markdown$Markdown$Table$Table, headers, _List_Nil)),
												rest);
										} else {
											return A2(
												$elm$core$List$cons,
												$dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
													$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(
														A3($dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith, '\n', rawHeaders, text.raw))),
												rest);
										}
									default:
										break _v0$9;
								}
							case 'Table':
								if (_v0.a.$ === 'Table') {
									var updatedTable = _v0.a.a;
									var _v15 = _v0.b;
									var rest = _v15.b;
									return A2(
										$elm$core$List$cons,
										$dillonkearns$elm_markdown$Markdown$RawBlock$Table(updatedTable),
										rest);
								} else {
									break _v0$9;
								}
							default:
								break _v0$9;
						}
					} else {
						break _v0$9;
					}
				}
				return A2($elm$core$List$cons, newRawBlock, state.rawBlocks);
			}()
		};
	});
var $dillonkearns$elm_markdown$Markdown$Parser$deadEndsToString = function (deadEnds) {
	return A2(
		$elm$core$String$join,
		'\n',
		A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$deadEndToString, deadEnds));
};
var $dillonkearns$elm_markdown$HtmlParser$Cdata = function (a) {
	return {$: 'Cdata', a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$Element = F3(
	function (a, b, c) {
		return {$: 'Element', a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$HtmlParser$Text = function (a) {
	return {$: 'Text', a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$expectTagNameCharacter = $elm$parser$Parser$Expecting('at least 1 tag name character');
var $dillonkearns$elm_markdown$HtmlParser$tagNameCharacter = function (c) {
	switch (c.valueOf()) {
		case ' ':
			return false;
		case '\r':
			return false;
		case '\n':
			return false;
		case '\t':
			return false;
		case '/':
			return false;
		case '<':
			return false;
		case '>':
			return false;
		case '\"':
			return false;
		case '\'':
			return false;
		case '=':
			return false;
		default:
			return true;
	}
};
var $dillonkearns$elm_markdown$HtmlParser$tagName = A2(
	$elm$parser$Parser$Advanced$mapChompedString,
	F2(
		function (name, _v0) {
			return $elm$core$String$toLower(name);
		}),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2($elm$parser$Parser$Advanced$chompIf, $dillonkearns$elm_markdown$HtmlParser$tagNameCharacter, $dillonkearns$elm_markdown$HtmlParser$expectTagNameCharacter),
		$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$HtmlParser$tagNameCharacter)));
var $dillonkearns$elm_markdown$HtmlParser$attributeName = $dillonkearns$elm_markdown$HtmlParser$tagName;
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$token(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $dillonkearns$elm_markdown$HtmlParser$entities = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'amp',
			_Utils_chr('&')),
			_Utils_Tuple2(
			'lt',
			_Utils_chr('<')),
			_Utils_Tuple2(
			'gt',
			_Utils_chr('>')),
			_Utils_Tuple2(
			'apos',
			_Utils_chr('\'')),
			_Utils_Tuple2(
			'quot',
			_Utils_chr('\"'))
		]));
var $elm$core$Char$fromCode = _Char_fromCode;
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Basics$pow = _Basics_pow;
var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return $elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2($elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return $elm$core$Result$Err(
							$elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $rtfeldman$elm_hex$Hex$fromString = function (str) {
	if ($elm$core$String$isEmpty(str)) {
		return $elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2($elm$core$String$startsWith, '-', str)) {
				var list = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$tail(
						$elm$core$String$toList(str)));
				return A2(
					$elm$core$Result$map,
					$elm$core$Basics$negate,
					A3(
						$rtfeldman$elm_hex$Hex$fromStringHelp,
						$elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					$rtfeldman$elm_hex$Hex$fromStringHelp,
					$elm$core$String$length(str) - 1,
					$elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2($elm$core$Result$mapError, formatError, result);
	}
};
var $dillonkearns$elm_markdown$HtmlParser$decodeEscape = function (s) {
	return A2($elm$core$String$startsWith, '#x', s) ? A2(
		$elm$core$Result$mapError,
		$elm$parser$Parser$Problem,
		A2(
			$elm$core$Result$map,
			$elm$core$Char$fromCode,
			$rtfeldman$elm_hex$Hex$fromString(
				A2($elm$core$String$dropLeft, 2, s)))) : (A2($elm$core$String$startsWith, '#', s) ? A2(
		$elm$core$Result$fromMaybe,
		$elm$parser$Parser$Problem('Invalid escaped character: ' + s),
		A2(
			$elm$core$Maybe$map,
			$elm$core$Char$fromCode,
			$elm$core$String$toInt(
				A2($elm$core$String$dropLeft, 1, s)))) : A2(
		$elm$core$Result$fromMaybe,
		$elm$parser$Parser$Problem('No entity named \"&' + (s + ';\" found.')),
		A2($elm$core$Dict$get, s, $dillonkearns$elm_markdown$HtmlParser$entities)));
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $dillonkearns$elm_markdown$HtmlParser$escapedChar = function (end_) {
	var process = function (entityStr) {
		var _v0 = $dillonkearns$elm_markdown$HtmlParser$decodeEscape(entityStr);
		if (_v0.$ === 'Ok') {
			var c = _v0.a;
			return $elm$parser$Parser$Advanced$succeed(c);
		} else {
			var e = _v0.a;
			return $elm$parser$Parser$Advanced$problem(e);
		}
	};
	var isEntityChar = function (c) {
		return (!_Utils_eq(c, end_)) && (!_Utils_eq(
			c,
			_Utils_chr(';')));
	};
	return A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$dillonkearns$elm_markdown$HtmlParser$symbol('&')),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$andThen,
				process,
				$elm$parser$Parser$Advanced$getChompedString(
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$chompIf,
							isEntityChar,
							$elm$parser$Parser$Expecting('an entity character')),
						$elm$parser$Parser$Advanced$chompWhile(isEntityChar)))),
			$dillonkearns$elm_markdown$HtmlParser$symbol(';')));
};
var $dillonkearns$elm_markdown$HtmlParser$textStringStep = F3(
	function (closingChar, predicate, accum) {
		return A2(
			$elm$parser$Parser$Advanced$andThen,
			function (soFar) {
				return $elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$Advanced$map,
							function (escaped) {
								return $elm$parser$Parser$Advanced$Loop(
									_Utils_ap(
										accum,
										_Utils_ap(
											soFar,
											$elm$core$String$fromChar(escaped))));
							},
							$dillonkearns$elm_markdown$HtmlParser$escapedChar(closingChar)),
							$elm$parser$Parser$Advanced$succeed(
							$elm$parser$Parser$Advanced$Done(
								_Utils_ap(accum, soFar)))
						]));
			},
			$elm$parser$Parser$Advanced$getChompedString(
				$elm$parser$Parser$Advanced$chompWhile(predicate)));
	});
var $dillonkearns$elm_markdown$HtmlParser$textString = function (closingChar) {
	var predicate = function (c) {
		return (!_Utils_eq(c, closingChar)) && (!_Utils_eq(
			c,
			_Utils_chr('&')));
	};
	return A2(
		$elm$parser$Parser$Advanced$loop,
		'',
		A2($dillonkearns$elm_markdown$HtmlParser$textStringStep, closingChar, predicate));
};
var $dillonkearns$elm_markdown$HtmlParser$attributeValue = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\"')),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$dillonkearns$elm_markdown$HtmlParser$textString(
					_Utils_chr('\"')),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\"'))),
			A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\'')),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$dillonkearns$elm_markdown$HtmlParser$textString(
					_Utils_chr('\'')),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\'')))
		]));
var $dillonkearns$elm_markdown$HtmlParser$keepOldest = F2(
	function (_new, mValue) {
		if (mValue.$ === 'Just') {
			var v = mValue.a;
			return $elm$core$Maybe$Just(v);
		} else {
			return $elm$core$Maybe$Just(_new);
		}
	});
var $dillonkearns$elm_markdown$HtmlParser$isWhitespace = function (c) {
	switch (c.valueOf()) {
		case ' ':
			return true;
		case '\r':
			return true;
		case '\n':
			return true;
		case '\t':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$HtmlParser$whiteSpace = $elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$HtmlParser$isWhitespace);
var $dillonkearns$elm_markdown$HtmlParser$attributesStep = function (attrs) {
	var process = F2(
		function (name, value) {
			return $elm$parser$Parser$Advanced$Loop(
				A3(
					$elm$core$Dict$update,
					$elm$core$String$toLower(name),
					$dillonkearns$elm_markdown$HtmlParser$keepOldest(value),
					attrs));
		});
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$keeper,
					$elm$parser$Parser$Advanced$succeed(process),
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2($elm$parser$Parser$Advanced$ignorer, $dillonkearns$elm_markdown$HtmlParser$attributeName, $dillonkearns$elm_markdown$HtmlParser$whiteSpace),
							$dillonkearns$elm_markdown$HtmlParser$symbol('=')),
						$dillonkearns$elm_markdown$HtmlParser$whiteSpace)),
				A2($elm$parser$Parser$Advanced$ignorer, $dillonkearns$elm_markdown$HtmlParser$attributeValue, $dillonkearns$elm_markdown$HtmlParser$whiteSpace)),
				$elm$parser$Parser$Advanced$succeed(
				$elm$parser$Parser$Advanced$Done(attrs))
			]));
};
var $dillonkearns$elm_markdown$HtmlParser$attributes = A2(
	$elm$parser$Parser$Advanced$map,
	A2(
		$elm$core$Dict$foldl,
		F3(
			function (key, value, accum) {
				return A2(
					$elm$core$List$cons,
					{name: key, value: value},
					accum);
			}),
		_List_Nil),
	A2($elm$parser$Parser$Advanced$loop, $elm$core$Dict$empty, $dillonkearns$elm_markdown$HtmlParser$attributesStep));
var $elm$parser$Parser$Advanced$chompUntilEndOr = function (str) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v0 = A5(_Parser_findSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v0.a;
			var newRow = _v0.b;
			var newCol = _v0.c;
			var adjustedOffset = (newOffset < 0) ? $elm$core$String$length(s.src) : newOffset;
			return A3(
				$elm$parser$Parser$Advanced$Good,
				_Utils_cmp(s.offset, adjustedOffset) < 0,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: adjustedOffset, row: newRow, src: s.src});
		});
};
var $dillonkearns$elm_markdown$HtmlParser$cdata = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
		$dillonkearns$elm_markdown$HtmlParser$symbol('<![CDATA[')),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString(
			$elm$parser$Parser$Advanced$chompUntilEndOr(']]>')),
		$dillonkearns$elm_markdown$HtmlParser$symbol(']]>')));
var $dillonkearns$elm_markdown$HtmlParser$childrenStep = F2(
	function (options, accum) {
		return A2(
			$elm$parser$Parser$Advanced$map,
			function (f) {
				return f(accum);
			},
			$elm$parser$Parser$Advanced$oneOf(options));
	});
var $dillonkearns$elm_markdown$HtmlParser$fail = function (str) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(str));
};
var $dillonkearns$elm_markdown$HtmlParser$closingTag = function (startTagName) {
	var closingTagName = A2(
		$elm$parser$Parser$Advanced$andThen,
		function (endTagName) {
			return _Utils_eq(startTagName, endTagName) ? $elm$parser$Parser$Advanced$succeed(_Utils_Tuple0) : $dillonkearns$elm_markdown$HtmlParser$fail('tag name mismatch: ' + (startTagName + (' and ' + endTagName)));
		},
		$dillonkearns$elm_markdown$HtmlParser$tagName);
	return A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$dillonkearns$elm_markdown$HtmlParser$symbol('</'),
					$dillonkearns$elm_markdown$HtmlParser$whiteSpace),
				closingTagName),
			$dillonkearns$elm_markdown$HtmlParser$whiteSpace),
		$dillonkearns$elm_markdown$HtmlParser$symbol('>'));
};
var $dillonkearns$elm_markdown$HtmlParser$Comment = function (a) {
	return {$: 'Comment', a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $dillonkearns$elm_markdown$HtmlParser$comment = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$HtmlParser$Comment),
		$elm$parser$Parser$Advanced$token(
			$dillonkearns$elm_markdown$HtmlParser$toToken('<!--'))),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString(
			$elm$parser$Parser$Advanced$chompUntilEndOr('-->')),
		$elm$parser$Parser$Advanced$token(
			$dillonkearns$elm_markdown$HtmlParser$toToken('-->'))));
var $dillonkearns$elm_markdown$HtmlParser$Declaration = F2(
	function (a, b) {
		return {$: 'Declaration', a: a, b: b};
	});
var $dillonkearns$elm_markdown$HtmlParser$expectUppercaseCharacter = $elm$parser$Parser$Expecting('at least 1 uppercase character');
var $dillonkearns$elm_markdown$HtmlParser$allUppercase = $elm$parser$Parser$Advanced$getChompedString(
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2($elm$parser$Parser$Advanced$chompIf, $elm$core$Char$isUpper, $dillonkearns$elm_markdown$HtmlParser$expectUppercaseCharacter),
		$elm$parser$Parser$Advanced$chompWhile($elm$core$Char$isUpper)));
var $dillonkearns$elm_markdown$HtmlParser$oneOrMoreWhiteSpace = A2(
	$elm$parser$Parser$Advanced$ignorer,
	A2(
		$elm$parser$Parser$Advanced$chompIf,
		$dillonkearns$elm_markdown$HtmlParser$isWhitespace,
		$elm$parser$Parser$Expecting('at least one whitespace')),
	$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$HtmlParser$isWhitespace));
var $dillonkearns$elm_markdown$HtmlParser$docType = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$HtmlParser$Declaration),
			$dillonkearns$elm_markdown$HtmlParser$symbol('<!')),
		A2($elm$parser$Parser$Advanced$ignorer, $dillonkearns$elm_markdown$HtmlParser$allUppercase, $dillonkearns$elm_markdown$HtmlParser$oneOrMoreWhiteSpace)),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString(
			$elm$parser$Parser$Advanced$chompUntilEndOr('>')),
		$dillonkearns$elm_markdown$HtmlParser$symbol('>')));
var $dillonkearns$elm_markdown$HtmlParser$ProcessingInstruction = function (a) {
	return {$: 'ProcessingInstruction', a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$processingInstruction = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$HtmlParser$ProcessingInstruction),
		$dillonkearns$elm_markdown$HtmlParser$symbol('<?')),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString(
			$elm$parser$Parser$Advanced$chompUntilEndOr('?>')),
		$dillonkearns$elm_markdown$HtmlParser$symbol('?>')));
var $dillonkearns$elm_markdown$HtmlParser$isNotTextNodeIgnoreChar = function (c) {
	switch (c.valueOf()) {
		case '<':
			return false;
		case '&':
			return false;
		default:
			return true;
	}
};
var $dillonkearns$elm_markdown$HtmlParser$textNodeStringStepOptions = _List_fromArray(
	[
		A2(
		$elm$parser$Parser$Advanced$map,
		function (_v0) {
			return $elm$parser$Parser$Advanced$Loop(_Utils_Tuple0);
		},
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$chompIf,
				$dillonkearns$elm_markdown$HtmlParser$isNotTextNodeIgnoreChar,
				$elm$parser$Parser$Expecting('is not & or <')),
			$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$HtmlParser$isNotTextNodeIgnoreChar))),
		A2(
		$elm$parser$Parser$Advanced$map,
		function (_v1) {
			return $elm$parser$Parser$Advanced$Loop(_Utils_Tuple0);
		},
		$dillonkearns$elm_markdown$HtmlParser$escapedChar(
			_Utils_chr('<'))),
		$elm$parser$Parser$Advanced$succeed(
		$elm$parser$Parser$Advanced$Done(_Utils_Tuple0))
	]);
var $dillonkearns$elm_markdown$HtmlParser$textNodeStringStep = function (_v0) {
	return $elm$parser$Parser$Advanced$oneOf($dillonkearns$elm_markdown$HtmlParser$textNodeStringStepOptions);
};
var $dillonkearns$elm_markdown$HtmlParser$textNodeString = $elm$parser$Parser$Advanced$getChompedString(
	A2($elm$parser$Parser$Advanced$loop, _Utils_Tuple0, $dillonkearns$elm_markdown$HtmlParser$textNodeStringStep));
var $dillonkearns$elm_markdown$HtmlParser$children = function (startTagName) {
	return A2(
		$elm$parser$Parser$Advanced$loop,
		_List_Nil,
		$dillonkearns$elm_markdown$HtmlParser$childrenStep(
			$dillonkearns$elm_markdown$HtmlParser$childrenStepOptions(startTagName)));
};
var $dillonkearns$elm_markdown$HtmlParser$childrenStepOptions = function (startTagName) {
	return _List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$map,
			F2(
				function (_v1, accum) {
					return $elm$parser$Parser$Advanced$Done(
						$elm$core$List$reverse(accum));
				}),
			$dillonkearns$elm_markdown$HtmlParser$closingTag(startTagName)),
			A2(
			$elm$parser$Parser$Advanced$andThen,
			function (text) {
				return $elm$core$String$isEmpty(text) ? A2(
					$elm$parser$Parser$Advanced$map,
					F2(
						function (_v2, accum) {
							return $elm$parser$Parser$Advanced$Done(
								$elm$core$List$reverse(accum));
						}),
					$dillonkearns$elm_markdown$HtmlParser$closingTag(startTagName)) : $elm$parser$Parser$Advanced$succeed(
					function (accum) {
						return $elm$parser$Parser$Advanced$Loop(
							A2(
								$elm$core$List$cons,
								$dillonkearns$elm_markdown$HtmlParser$Text(text),
								accum));
					});
			},
			$dillonkearns$elm_markdown$HtmlParser$textNodeString),
			A2(
			$elm$parser$Parser$Advanced$map,
			F2(
				function (_new, accum) {
					return $elm$parser$Parser$Advanced$Loop(
						A2($elm$core$List$cons, _new, accum));
				}),
			$dillonkearns$elm_markdown$HtmlParser$cyclic$html())
		]);
};
var $dillonkearns$elm_markdown$HtmlParser$elementContinuation = function (startTagName) {
	return A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed(
					$dillonkearns$elm_markdown$HtmlParser$Element(startTagName)),
				$dillonkearns$elm_markdown$HtmlParser$whiteSpace),
			A2($elm$parser$Parser$Advanced$ignorer, $dillonkearns$elm_markdown$HtmlParser$attributes, $dillonkearns$elm_markdown$HtmlParser$whiteSpace)),
		$elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return _List_Nil;
					},
					$dillonkearns$elm_markdown$HtmlParser$symbol('/>')),
					A2(
					$elm$parser$Parser$Advanced$keeper,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
						$dillonkearns$elm_markdown$HtmlParser$symbol('>')),
					$dillonkearns$elm_markdown$HtmlParser$children(startTagName))
				])));
};
function $dillonkearns$elm_markdown$HtmlParser$cyclic$html() {
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2($elm$parser$Parser$Advanced$map, $dillonkearns$elm_markdown$HtmlParser$Cdata, $dillonkearns$elm_markdown$HtmlParser$cdata),
				$dillonkearns$elm_markdown$HtmlParser$processingInstruction,
				$dillonkearns$elm_markdown$HtmlParser$comment,
				$dillonkearns$elm_markdown$HtmlParser$docType,
				$dillonkearns$elm_markdown$HtmlParser$cyclic$element()
			]));
}
function $dillonkearns$elm_markdown$HtmlParser$cyclic$element() {
	return A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$dillonkearns$elm_markdown$HtmlParser$symbol('<')),
		A2($elm$parser$Parser$Advanced$andThen, $dillonkearns$elm_markdown$HtmlParser$elementContinuation, $dillonkearns$elm_markdown$HtmlParser$tagName));
}
try {
	var $dillonkearns$elm_markdown$HtmlParser$html = $dillonkearns$elm_markdown$HtmlParser$cyclic$html();
	$dillonkearns$elm_markdown$HtmlParser$cyclic$html = function () {
		return $dillonkearns$elm_markdown$HtmlParser$html;
	};
	var $dillonkearns$elm_markdown$HtmlParser$element = $dillonkearns$elm_markdown$HtmlParser$cyclic$element();
	$dillonkearns$elm_markdown$HtmlParser$cyclic$element = function () {
		return $dillonkearns$elm_markdown$HtmlParser$element;
	};
} catch ($) {
	throw 'Some top-level definitions from `HtmlParser` are causing infinite recursion:\n\n  ┌─────┐\n  │    children\n  │     ↓\n  │    childrenStepOptions\n  │     ↓\n  │    html\n  │     ↓\n  │    element\n  │     ↓\n  │    elementContinuation\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $dillonkearns$elm_markdown$Parser$Token$tab = A2(
	$elm$parser$Parser$Advanced$Token,
	'\t',
	$elm$parser$Parser$Expecting('a tab'));
var $dillonkearns$elm_markdown$Markdown$Parser$exactlyFourSpaces = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$tab),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$backtrackable(
				$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$space)),
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						$elm$parser$Parser$Advanced$symbol(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'   ',
							$elm$parser$Parser$ExpectingSymbol('Indentation'))),
						$elm$parser$Parser$Advanced$symbol(
						A2(
							$elm$parser$Parser$Advanced$Token,
							' \t',
							$elm$parser$Parser$ExpectingSymbol('Indentation'))),
						$elm$parser$Parser$Advanced$symbol(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'  \t',
							$elm$parser$Parser$ExpectingSymbol('Indentation')))
					])))
		]));
var $dillonkearns$elm_markdown$Markdown$Parser$indentedCodeBlock = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$RawBlock$IndentedCodeBlock),
		$dillonkearns$elm_markdown$Markdown$Parser$exactlyFourSpaces),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString($dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
		$dillonkearns$elm_markdown$Helpers$lineEndOrEnd));
var $dillonkearns$elm_markdown$Markdown$Parser$innerParagraphParser = A2(
	$elm$parser$Parser$Advanced$mapChompedString,
	F2(
		function (rawLine, _v0) {
			return $dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
				$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(rawLine));
		}),
	$dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd);
var $dillonkearns$elm_markdown$Markdown$Parser$openBlockOrParagraphParser = A2($elm$parser$Parser$Advanced$ignorer, $dillonkearns$elm_markdown$Markdown$Parser$innerParagraphParser, $dillonkearns$elm_markdown$Helpers$lineEndOrEnd);
var $dillonkearns$elm_markdown$Markdown$RawBlock$OrderedListBlock = F2(
	function (a, b) {
		return {$: 'OrderedListBlock', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore = function (condition) {
	return A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$chompIf,
			condition,
			$elm$parser$Parser$Problem('Expected one or more character')),
		$elm$parser$Parser$Advanced$chompWhile(condition));
};
var $dillonkearns$elm_markdown$Parser$Token$closingParen = A2(
	$elm$parser$Parser$Advanced$Token,
	')',
	$elm$parser$Parser$Expecting('a `)`'));
var $dillonkearns$elm_markdown$Parser$Token$dot = A2(
	$elm$parser$Parser$Advanced$Token,
	'.',
	$elm$parser$Parser$Expecting('a `.`'));
var $dillonkearns$elm_markdown$Markdown$OrderedList$itemBody = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab)),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$getChompedString($dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
				$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(''),
			$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)
		]));
var $dillonkearns$elm_markdown$Parser$Extra$positiveInteger = A2(
	$elm$parser$Parser$Advanced$mapChompedString,
	F2(
		function (str, _v0) {
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(str));
		}),
	$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore($elm$core$Char$isDigit));
var $dillonkearns$elm_markdown$Markdown$OrderedList$singleItemParser = function (listMarker) {
	return A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$elm$parser$Parser$Advanced$backtrackable(
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$dillonkearns$elm_markdown$Parser$Extra$positiveInteger,
					$elm$parser$Parser$Advanced$symbol(listMarker)))),
		$dillonkearns$elm_markdown$Markdown$OrderedList$itemBody);
};
var $dillonkearns$elm_markdown$Markdown$OrderedList$statementsHelp = F2(
	function (itemParser, revStmts) {
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$Advanced$map,
					function (stmt) {
						return $elm$parser$Parser$Advanced$Loop(
							A2($elm$core$List$cons, stmt, revStmts));
					},
					itemParser),
					$elm$parser$Parser$Advanced$succeed(
					$elm$parser$Parser$Advanced$Done(
						$elm$core$List$reverse(revStmts)))
				]));
	});
var $dillonkearns$elm_markdown$Markdown$OrderedList$parseSubsequentItems = F3(
	function (startingIndex, listMarker, firstItem) {
		return A2(
			$elm$parser$Parser$Advanced$map,
			function (items) {
				return _Utils_Tuple2(
					startingIndex,
					A2($elm$core$List$cons, firstItem, items));
			},
			A2(
				$elm$parser$Parser$Advanced$loop,
				_List_Nil,
				$dillonkearns$elm_markdown$Markdown$OrderedList$statementsHelp(
					$dillonkearns$elm_markdown$Markdown$OrderedList$singleItemParser(listMarker))));
	});
var $dillonkearns$elm_markdown$Markdown$OrderedList$positiveIntegerMaxOf9Digits = A2(
	$elm$parser$Parser$Advanced$andThen,
	function (parsed) {
		return (parsed <= 999999999) ? $elm$parser$Parser$Advanced$succeed(parsed) : $elm$parser$Parser$Advanced$problem(
			$elm$parser$Parser$Problem('Starting numbers must be nine digits or less.'));
	},
	$dillonkearns$elm_markdown$Parser$Extra$positiveInteger);
var $dillonkearns$elm_markdown$Markdown$OrderedList$validateStartsWith1 = function (parsed) {
	if (parsed === 1) {
		return $elm$parser$Parser$Advanced$succeed(parsed);
	} else {
		return $elm$parser$Parser$Advanced$problem(
			$elm$parser$Parser$Problem('Lists inside a paragraph or after a paragraph without a blank line must start with 1'));
	}
};
var $dillonkearns$elm_markdown$Markdown$OrderedList$parser = function (previousWasBody) {
	return A2(
		$elm$parser$Parser$Advanced$andThen,
		$elm$core$Basics$identity,
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$keeper,
					$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$OrderedList$parseSubsequentItems),
					$elm$parser$Parser$Advanced$backtrackable(
						previousWasBody ? A2($elm$parser$Parser$Advanced$andThen, $dillonkearns$elm_markdown$Markdown$OrderedList$validateStartsWith1, $dillonkearns$elm_markdown$Markdown$OrderedList$positiveIntegerMaxOf9Digits) : $dillonkearns$elm_markdown$Markdown$OrderedList$positiveIntegerMaxOf9Digits)),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$backtrackable(
						$elm$parser$Parser$Advanced$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$Advanced$ignorer,
									$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Parser$Token$dot),
									$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$dot)),
									A2(
									$elm$parser$Parser$Advanced$ignorer,
									$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Parser$Token$closingParen),
									$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$closingParen))
								]))),
					$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab))),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$getChompedString($dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
				$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)));
};
var $dillonkearns$elm_markdown$Markdown$Parser$orderedListBlock = function (previousWasBody) {
	return A2(
		$elm$parser$Parser$Advanced$map,
		function (_v0) {
			var startingIndex = _v0.a;
			var unparsedLines = _v0.b;
			return A2(
				$dillonkearns$elm_markdown$Markdown$RawBlock$OrderedListBlock,
				startingIndex,
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines, unparsedLines));
		},
		$dillonkearns$elm_markdown$Markdown$OrderedList$parser(previousWasBody));
};
var $dillonkearns$elm_markdown$Markdown$Inline$CodeInline = function (a) {
	return {$: 'CodeInline', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Inline$Emphasis = F2(
	function (a, b) {
		return {$: 'Emphasis', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Inline$HardLineBreak = {$: 'HardLineBreak'};
var $dillonkearns$elm_markdown$Markdown$Inline$HtmlInline = function (a) {
	return {$: 'HtmlInline', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Inline$Image = F3(
	function (a, b, c) {
		return {$: 'Image', a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Inline$Link = F3(
	function (a, b, c) {
		return {$: 'Link', a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Inline$Strikethrough = function (a) {
	return {$: 'Strikethrough', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Inline$Text = function (a) {
	return {$: 'Text', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$matchToInline = function (_v0) {
	var match = _v0.a;
	var _v1 = match.type_;
	switch (_v1.$) {
		case 'NormalType':
			return $dillonkearns$elm_markdown$Markdown$Inline$Text(match.text);
		case 'HardLineBreakType':
			return $dillonkearns$elm_markdown$Markdown$Inline$HardLineBreak;
		case 'CodeType':
			return $dillonkearns$elm_markdown$Markdown$Inline$CodeInline(match.text);
		case 'AutolinkType':
			var _v2 = _v1.a;
			var text = _v2.a;
			var url = _v2.b;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Inline$Link,
				url,
				$elm$core$Maybe$Nothing,
				_List_fromArray(
					[
						$dillonkearns$elm_markdown$Markdown$Inline$Text(text)
					]));
		case 'LinkType':
			var _v3 = _v1.a;
			var url = _v3.a;
			var maybeTitle = _v3.b;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Inline$Link,
				url,
				maybeTitle,
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
		case 'ImageType':
			var _v4 = _v1.a;
			var url = _v4.a;
			var maybeTitle = _v4.b;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Inline$Image,
				url,
				maybeTitle,
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
		case 'HtmlType':
			var model = _v1.a;
			return $dillonkearns$elm_markdown$Markdown$Inline$HtmlInline(model);
		case 'EmphasisType':
			var length = _v1.a;
			return A2(
				$dillonkearns$elm_markdown$Markdown$Inline$Emphasis,
				length,
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
		default:
			return $dillonkearns$elm_markdown$Markdown$Inline$Strikethrough(
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines = function (matches) {
	return A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$InlineParser$matchToInline, matches);
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$Match = function (a) {
	return {$: 'Match', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$prepareChildMatch = F2(
	function (parentMatch, childMatch) {
		return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			{end: childMatch.end - parentMatch.textStart, matches: childMatch.matches, start: childMatch.start - parentMatch.textStart, text: childMatch.text, textEnd: childMatch.textEnd - parentMatch.textStart, textStart: childMatch.textStart - parentMatch.textStart, type_: childMatch.type_});
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$addChild = F2(
	function (parentMatch, childMatch) {
		return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			{
				end: parentMatch.end,
				matches: A2(
					$elm$core$List$cons,
					A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareChildMatch, parentMatch, childMatch),
					parentMatch.matches),
				start: parentMatch.start,
				text: parentMatch.text,
				textEnd: parentMatch.textEnd,
				textStart: parentMatch.textStart,
				type_: parentMatch.type_
			});
	});
var $elm$core$List$sortBy = _List_sortBy;
var $dillonkearns$elm_markdown$Markdown$InlineParser$organizeChildren = function (_v4) {
	var match = _v4.a;
	return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
		{
			end: match.end,
			matches: $dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatches(match.matches),
			start: match.start,
			text: match.text,
			textEnd: match.textEnd,
			textStart: match.textStart,
			type_: match.type_
		});
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatches = function (matches) {
	var _v2 = A2(
		$elm$core$List$sortBy,
		function (_v3) {
			var match = _v3.a;
			return match.start;
		},
		matches);
	if (!_v2.b) {
		return _List_Nil;
	} else {
		var first = _v2.a;
		var rest = _v2.b;
		return A3($dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatchesHelp, rest, first, _List_Nil);
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatchesHelp = F3(
	function (remaining, _v0, matchesTail) {
		organizeMatchesHelp:
		while (true) {
			var prevMatch = _v0.a;
			if (!remaining.b) {
				return A2(
					$elm$core$List$cons,
					$dillonkearns$elm_markdown$Markdown$InlineParser$organizeChildren(
						$dillonkearns$elm_markdown$Markdown$InlineParser$Match(prevMatch)),
					matchesTail);
			} else {
				var match = remaining.a.a;
				var rest = remaining.b;
				if (_Utils_cmp(prevMatch.end, match.start) < 1) {
					var $temp$remaining = rest,
						$temp$_v0 = $dillonkearns$elm_markdown$Markdown$InlineParser$Match(match),
						$temp$matchesTail = A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$InlineParser$organizeChildren(
							$dillonkearns$elm_markdown$Markdown$InlineParser$Match(prevMatch)),
						matchesTail);
					remaining = $temp$remaining;
					_v0 = $temp$_v0;
					matchesTail = $temp$matchesTail;
					continue organizeMatchesHelp;
				} else {
					if ((_Utils_cmp(prevMatch.start, match.start) < 0) && (_Utils_cmp(prevMatch.end, match.end) > 0)) {
						var $temp$remaining = rest,
							$temp$_v0 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$addChild, prevMatch, match),
							$temp$matchesTail = matchesTail;
						remaining = $temp$remaining;
						_v0 = $temp$_v0;
						matchesTail = $temp$matchesTail;
						continue organizeMatchesHelp;
					} else {
						var $temp$remaining = rest,
							$temp$_v0 = $dillonkearns$elm_markdown$Markdown$InlineParser$Match(prevMatch),
							$temp$matchesTail = matchesTail;
						remaining = $temp$remaining;
						_v0 = $temp$_v0;
						matchesTail = $temp$matchesTail;
						continue organizeMatchesHelp;
					}
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$NormalType = {$: 'NormalType'};
var $dillonkearns$elm_markdown$Markdown$Helpers$containsAmpersand = function (string) {
	return A2($elm$core$String$contains, '&', string);
};
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $dillonkearns$elm_markdown$Markdown$Entity$decimalRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('&#([0-9]{1,8});'));
var $elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var $elm$core$Basics$modBy = _Basics_modBy;
var $dillonkearns$elm_markdown$Markdown$Entity$isBadEndUnicode = function (_int) {
	var remain_ = A2($elm$core$Basics$modBy, 16, _int);
	var remain = A2($elm$core$Basics$modBy, 131070, _int);
	return (_int >= 131070) && ((((0 <= remain) && (remain <= 15)) || ((65536 <= remain) && (remain <= 65551))) && ((remain_ === 14) || (remain_ === 15)));
};
var $dillonkearns$elm_markdown$Markdown$Entity$isValidUnicode = function (_int) {
	return (_int === 9) || ((_int === 10) || ((_int === 13) || ((_int === 133) || (((32 <= _int) && (_int <= 126)) || (((160 <= _int) && (_int <= 55295)) || (((57344 <= _int) && (_int <= 64975)) || (((65008 <= _int) && (_int <= 65533)) || ((65536 <= _int) && (_int <= 1114109)))))))));
};
var $dillonkearns$elm_markdown$Markdown$Entity$validUnicode = function (_int) {
	return ($dillonkearns$elm_markdown$Markdown$Entity$isValidUnicode(_int) && (!$dillonkearns$elm_markdown$Markdown$Entity$isBadEndUnicode(_int))) ? $elm$core$String$fromChar(
		$elm$core$Char$fromCode(_int)) : $elm$core$String$fromChar(
		$elm$core$Char$fromCode(65533));
};
var $dillonkearns$elm_markdown$Markdown$Entity$replaceDecimal = function (match) {
	var _v0 = match.submatches;
	if (_v0.b && (_v0.a.$ === 'Just')) {
		var first = _v0.a.a;
		var _v1 = $elm$core$String$toInt(first);
		if (_v1.$ === 'Just') {
			var v = _v1.a;
			return $dillonkearns$elm_markdown$Markdown$Entity$validUnicode(v);
		} else {
			return match.match;
		}
	} else {
		return match.match;
	}
};
var $dillonkearns$elm_markdown$Markdown$Entity$replaceDecimals = A2($elm$regex$Regex$replace, $dillonkearns$elm_markdown$Markdown$Entity$decimalRegex, $dillonkearns$elm_markdown$Markdown$Entity$replaceDecimal);
var $dillonkearns$elm_markdown$Markdown$Entity$entitiesRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('&([0-9a-zA-Z]+);'));
var $dillonkearns$elm_markdown$Markdown$Entity$entities = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('quot', 34),
			_Utils_Tuple2('amp', 38),
			_Utils_Tuple2('apos', 39),
			_Utils_Tuple2('lt', 60),
			_Utils_Tuple2('gt', 62),
			_Utils_Tuple2('nbsp', 160),
			_Utils_Tuple2('iexcl', 161),
			_Utils_Tuple2('cent', 162),
			_Utils_Tuple2('pound', 163),
			_Utils_Tuple2('curren', 164),
			_Utils_Tuple2('yen', 165),
			_Utils_Tuple2('brvbar', 166),
			_Utils_Tuple2('sect', 167),
			_Utils_Tuple2('uml', 168),
			_Utils_Tuple2('copy', 169),
			_Utils_Tuple2('ordf', 170),
			_Utils_Tuple2('laquo', 171),
			_Utils_Tuple2('not', 172),
			_Utils_Tuple2('shy', 173),
			_Utils_Tuple2('reg', 174),
			_Utils_Tuple2('macr', 175),
			_Utils_Tuple2('deg', 176),
			_Utils_Tuple2('plusmn', 177),
			_Utils_Tuple2('sup2', 178),
			_Utils_Tuple2('sup3', 179),
			_Utils_Tuple2('acute', 180),
			_Utils_Tuple2('micro', 181),
			_Utils_Tuple2('para', 182),
			_Utils_Tuple2('middot', 183),
			_Utils_Tuple2('cedil', 184),
			_Utils_Tuple2('sup1', 185),
			_Utils_Tuple2('ordm', 186),
			_Utils_Tuple2('raquo', 187),
			_Utils_Tuple2('frac14', 188),
			_Utils_Tuple2('frac12', 189),
			_Utils_Tuple2('frac34', 190),
			_Utils_Tuple2('iquest', 191),
			_Utils_Tuple2('Agrave', 192),
			_Utils_Tuple2('Aacute', 193),
			_Utils_Tuple2('Acirc', 194),
			_Utils_Tuple2('Atilde', 195),
			_Utils_Tuple2('Auml', 196),
			_Utils_Tuple2('Aring', 197),
			_Utils_Tuple2('AElig', 198),
			_Utils_Tuple2('Ccedil', 199),
			_Utils_Tuple2('Egrave', 200),
			_Utils_Tuple2('Eacute', 201),
			_Utils_Tuple2('Ecirc', 202),
			_Utils_Tuple2('Euml', 203),
			_Utils_Tuple2('Igrave', 204),
			_Utils_Tuple2('Iacute', 205),
			_Utils_Tuple2('Icirc', 206),
			_Utils_Tuple2('Iuml', 207),
			_Utils_Tuple2('ETH', 208),
			_Utils_Tuple2('Ntilde', 209),
			_Utils_Tuple2('Ograve', 210),
			_Utils_Tuple2('Oacute', 211),
			_Utils_Tuple2('Ocirc', 212),
			_Utils_Tuple2('Otilde', 213),
			_Utils_Tuple2('Ouml', 214),
			_Utils_Tuple2('times', 215),
			_Utils_Tuple2('Oslash', 216),
			_Utils_Tuple2('Ugrave', 217),
			_Utils_Tuple2('Uacute', 218),
			_Utils_Tuple2('Ucirc', 219),
			_Utils_Tuple2('Uuml', 220),
			_Utils_Tuple2('Yacute', 221),
			_Utils_Tuple2('THORN', 222),
			_Utils_Tuple2('szlig', 223),
			_Utils_Tuple2('agrave', 224),
			_Utils_Tuple2('aacute', 225),
			_Utils_Tuple2('acirc', 226),
			_Utils_Tuple2('atilde', 227),
			_Utils_Tuple2('auml', 228),
			_Utils_Tuple2('aring', 229),
			_Utils_Tuple2('aelig', 230),
			_Utils_Tuple2('ccedil', 231),
			_Utils_Tuple2('egrave', 232),
			_Utils_Tuple2('eacute', 233),
			_Utils_Tuple2('ecirc', 234),
			_Utils_Tuple2('euml', 235),
			_Utils_Tuple2('igrave', 236),
			_Utils_Tuple2('iacute', 237),
			_Utils_Tuple2('icirc', 238),
			_Utils_Tuple2('iuml', 239),
			_Utils_Tuple2('eth', 240),
			_Utils_Tuple2('ntilde', 241),
			_Utils_Tuple2('ograve', 242),
			_Utils_Tuple2('oacute', 243),
			_Utils_Tuple2('ocirc', 244),
			_Utils_Tuple2('otilde', 245),
			_Utils_Tuple2('ouml', 246),
			_Utils_Tuple2('divide', 247),
			_Utils_Tuple2('oslash', 248),
			_Utils_Tuple2('ugrave', 249),
			_Utils_Tuple2('uacute', 250),
			_Utils_Tuple2('ucirc', 251),
			_Utils_Tuple2('uuml', 252),
			_Utils_Tuple2('yacute', 253),
			_Utils_Tuple2('thorn', 254),
			_Utils_Tuple2('yuml', 255),
			_Utils_Tuple2('OElig', 338),
			_Utils_Tuple2('oelig', 339),
			_Utils_Tuple2('Scaron', 352),
			_Utils_Tuple2('scaron', 353),
			_Utils_Tuple2('Yuml', 376),
			_Utils_Tuple2('fnof', 402),
			_Utils_Tuple2('circ', 710),
			_Utils_Tuple2('tilde', 732),
			_Utils_Tuple2('Alpha', 913),
			_Utils_Tuple2('Beta', 914),
			_Utils_Tuple2('Gamma', 915),
			_Utils_Tuple2('Delta', 916),
			_Utils_Tuple2('Epsilon', 917),
			_Utils_Tuple2('Zeta', 918),
			_Utils_Tuple2('Eta', 919),
			_Utils_Tuple2('Theta', 920),
			_Utils_Tuple2('Iota', 921),
			_Utils_Tuple2('Kappa', 922),
			_Utils_Tuple2('Lambda', 923),
			_Utils_Tuple2('Mu', 924),
			_Utils_Tuple2('Nu', 925),
			_Utils_Tuple2('Xi', 926),
			_Utils_Tuple2('Omicron', 927),
			_Utils_Tuple2('Pi', 928),
			_Utils_Tuple2('Rho', 929),
			_Utils_Tuple2('Sigma', 931),
			_Utils_Tuple2('Tau', 932),
			_Utils_Tuple2('Upsilon', 933),
			_Utils_Tuple2('Phi', 934),
			_Utils_Tuple2('Chi', 935),
			_Utils_Tuple2('Psi', 936),
			_Utils_Tuple2('Omega', 937),
			_Utils_Tuple2('alpha', 945),
			_Utils_Tuple2('beta', 946),
			_Utils_Tuple2('gamma', 947),
			_Utils_Tuple2('delta', 948),
			_Utils_Tuple2('epsilon', 949),
			_Utils_Tuple2('zeta', 950),
			_Utils_Tuple2('eta', 951),
			_Utils_Tuple2('theta', 952),
			_Utils_Tuple2('iota', 953),
			_Utils_Tuple2('kappa', 954),
			_Utils_Tuple2('lambda', 955),
			_Utils_Tuple2('mu', 956),
			_Utils_Tuple2('nu', 957),
			_Utils_Tuple2('xi', 958),
			_Utils_Tuple2('omicron', 959),
			_Utils_Tuple2('pi', 960),
			_Utils_Tuple2('rho', 961),
			_Utils_Tuple2('sigmaf', 962),
			_Utils_Tuple2('sigma', 963),
			_Utils_Tuple2('tau', 964),
			_Utils_Tuple2('upsilon', 965),
			_Utils_Tuple2('phi', 966),
			_Utils_Tuple2('chi', 967),
			_Utils_Tuple2('psi', 968),
			_Utils_Tuple2('omega', 969),
			_Utils_Tuple2('thetasym', 977),
			_Utils_Tuple2('upsih', 978),
			_Utils_Tuple2('piv', 982),
			_Utils_Tuple2('ensp', 8194),
			_Utils_Tuple2('emsp', 8195),
			_Utils_Tuple2('thinsp', 8201),
			_Utils_Tuple2('zwnj', 8204),
			_Utils_Tuple2('zwj', 8205),
			_Utils_Tuple2('lrm', 8206),
			_Utils_Tuple2('rlm', 8207),
			_Utils_Tuple2('ndash', 8211),
			_Utils_Tuple2('mdash', 8212),
			_Utils_Tuple2('lsquo', 8216),
			_Utils_Tuple2('rsquo', 8217),
			_Utils_Tuple2('sbquo', 8218),
			_Utils_Tuple2('ldquo', 8220),
			_Utils_Tuple2('rdquo', 8221),
			_Utils_Tuple2('bdquo', 8222),
			_Utils_Tuple2('dagger', 8224),
			_Utils_Tuple2('Dagger', 8225),
			_Utils_Tuple2('bull', 8226),
			_Utils_Tuple2('hellip', 8230),
			_Utils_Tuple2('permil', 8240),
			_Utils_Tuple2('prime', 8242),
			_Utils_Tuple2('Prime', 8243),
			_Utils_Tuple2('lsaquo', 8249),
			_Utils_Tuple2('rsaquo', 8250),
			_Utils_Tuple2('oline', 8254),
			_Utils_Tuple2('frasl', 8260),
			_Utils_Tuple2('euro', 8364),
			_Utils_Tuple2('image', 8465),
			_Utils_Tuple2('weierp', 8472),
			_Utils_Tuple2('real', 8476),
			_Utils_Tuple2('trade', 8482),
			_Utils_Tuple2('alefsym', 8501),
			_Utils_Tuple2('larr', 8592),
			_Utils_Tuple2('uarr', 8593),
			_Utils_Tuple2('rarr', 8594),
			_Utils_Tuple2('darr', 8595),
			_Utils_Tuple2('harr', 8596),
			_Utils_Tuple2('crarr', 8629),
			_Utils_Tuple2('lArr', 8656),
			_Utils_Tuple2('uArr', 8657),
			_Utils_Tuple2('rArr', 8658),
			_Utils_Tuple2('dArr', 8659),
			_Utils_Tuple2('hArr', 8660),
			_Utils_Tuple2('forall', 8704),
			_Utils_Tuple2('part', 8706),
			_Utils_Tuple2('exist', 8707),
			_Utils_Tuple2('empty', 8709),
			_Utils_Tuple2('nabla', 8711),
			_Utils_Tuple2('isin', 8712),
			_Utils_Tuple2('notin', 8713),
			_Utils_Tuple2('ni', 8715),
			_Utils_Tuple2('prod', 8719),
			_Utils_Tuple2('sum', 8721),
			_Utils_Tuple2('minus', 8722),
			_Utils_Tuple2('lowast', 8727),
			_Utils_Tuple2('radic', 8730),
			_Utils_Tuple2('prop', 8733),
			_Utils_Tuple2('infin', 8734),
			_Utils_Tuple2('ang', 8736),
			_Utils_Tuple2('and', 8743),
			_Utils_Tuple2('or', 8744),
			_Utils_Tuple2('cap', 8745),
			_Utils_Tuple2('cup', 8746),
			_Utils_Tuple2('int', 8747),
			_Utils_Tuple2('there4', 8756),
			_Utils_Tuple2('sim', 8764),
			_Utils_Tuple2('cong', 8773),
			_Utils_Tuple2('asymp', 8776),
			_Utils_Tuple2('ne', 8800),
			_Utils_Tuple2('equiv', 8801),
			_Utils_Tuple2('le', 8804),
			_Utils_Tuple2('ge', 8805),
			_Utils_Tuple2('sub', 8834),
			_Utils_Tuple2('sup', 8835),
			_Utils_Tuple2('nsub', 8836),
			_Utils_Tuple2('sube', 8838),
			_Utils_Tuple2('supe', 8839),
			_Utils_Tuple2('oplus', 8853),
			_Utils_Tuple2('otimes', 8855),
			_Utils_Tuple2('perp', 8869),
			_Utils_Tuple2('sdot', 8901),
			_Utils_Tuple2('lceil', 8968),
			_Utils_Tuple2('rceil', 8969),
			_Utils_Tuple2('lfloor', 8970),
			_Utils_Tuple2('rfloor', 8971),
			_Utils_Tuple2('lang', 9001),
			_Utils_Tuple2('rang', 9002),
			_Utils_Tuple2('loz', 9674),
			_Utils_Tuple2('spades', 9824),
			_Utils_Tuple2('clubs', 9827),
			_Utils_Tuple2('hearts', 9829),
			_Utils_Tuple2('diams', 9830)
		]));
var $dillonkearns$elm_markdown$Markdown$Entity$replaceEntity = function (match) {
	var _v0 = match.submatches;
	if (_v0.b && (_v0.a.$ === 'Just')) {
		var first = _v0.a.a;
		var _v1 = A2($elm$core$Dict$get, first, $dillonkearns$elm_markdown$Markdown$Entity$entities);
		if (_v1.$ === 'Just') {
			var code = _v1.a;
			return $elm$core$String$fromChar(
				$elm$core$Char$fromCode(code));
		} else {
			return match.match;
		}
	} else {
		return match.match;
	}
};
var $dillonkearns$elm_markdown$Markdown$Entity$replaceEntities = A2($elm$regex$Regex$replace, $dillonkearns$elm_markdown$Markdown$Entity$entitiesRegex, $dillonkearns$elm_markdown$Markdown$Entity$replaceEntity);
var $dillonkearns$elm_markdown$Markdown$Helpers$escapableRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\+)([!\"#$%&\\\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-])'));
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $dillonkearns$elm_markdown$Markdown$Helpers$replaceEscapable = A2(
	$elm$regex$Regex$replace,
	$dillonkearns$elm_markdown$Markdown$Helpers$escapableRegex,
	function (regexMatch) {
		var _v0 = regexMatch.submatches;
		if (((_v0.b && (_v0.a.$ === 'Just')) && _v0.b.b) && (_v0.b.a.$ === 'Just')) {
			var backslashes = _v0.a.a;
			var _v1 = _v0.b;
			var escapedStr = _v1.a.a;
			return _Utils_ap(
				A2(
					$elm$core$String$repeat,
					($elm$core$String$length(backslashes) / 2) | 0,
					'\\'),
				escapedStr);
		} else {
			return regexMatch.match;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Entity$hexadecimalRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('&#[Xx]([0-9a-fA-F]{1,8});'));
var $elm$core$String$foldl = _String_foldl;
var $dillonkearns$elm_markdown$Markdown$Entity$hexToInt = function (string) {
	var folder = F2(
		function (hexDigit, _int) {
			return ((_int * 16) + A2(
				$elm$core$Basics$modBy,
				39,
				$elm$core$Char$toCode(hexDigit))) - 9;
		});
	return A3(
		$elm$core$String$foldl,
		folder,
		0,
		$elm$core$String$toLower(string));
};
var $dillonkearns$elm_markdown$Markdown$Entity$replaceHexadecimal = function (match) {
	var _v0 = match.submatches;
	if (_v0.b && (_v0.a.$ === 'Just')) {
		var first = _v0.a.a;
		return $dillonkearns$elm_markdown$Markdown$Entity$validUnicode(
			$dillonkearns$elm_markdown$Markdown$Entity$hexToInt(first));
	} else {
		return match.match;
	}
};
var $dillonkearns$elm_markdown$Markdown$Entity$replaceHexadecimals = A2($elm$regex$Regex$replace, $dillonkearns$elm_markdown$Markdown$Entity$hexadecimalRegex, $dillonkearns$elm_markdown$Markdown$Entity$replaceHexadecimal);
var $dillonkearns$elm_markdown$Markdown$Helpers$formatStr = function (str) {
	var withEscapes = $dillonkearns$elm_markdown$Markdown$Helpers$replaceEscapable(str);
	return $dillonkearns$elm_markdown$Markdown$Helpers$containsAmpersand(withEscapes) ? $dillonkearns$elm_markdown$Markdown$Entity$replaceHexadecimals(
		$dillonkearns$elm_markdown$Markdown$Entity$replaceDecimals(
			$dillonkearns$elm_markdown$Markdown$Entity$replaceEntities(withEscapes))) : withEscapes;
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch = function (text) {
	return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
		{
			end: 0,
			matches: _List_Nil,
			start: 0,
			text: $dillonkearns$elm_markdown$Markdown$Helpers$formatStr(text),
			textEnd: 0,
			textStart: 0,
			type_: $dillonkearns$elm_markdown$Markdown$InlineParser$NormalType
		});
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatch = F3(
	function (rawText, _v2, parsedMatches) {
		var matchModel = _v2.a;
		var updtMatch = $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			{
				end: matchModel.end,
				matches: A3($dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatches, matchModel.text, _List_Nil, matchModel.matches),
				start: matchModel.start,
				text: matchModel.text,
				textEnd: matchModel.textEnd,
				textStart: matchModel.textStart,
				type_: matchModel.type_
			});
		if (!parsedMatches.b) {
			var finalStr = A2($elm$core$String$dropLeft, matchModel.end, rawText);
			return $elm$core$String$isEmpty(finalStr) ? _List_fromArray(
				[updtMatch]) : _List_fromArray(
				[
					updtMatch,
					$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(finalStr)
				]);
		} else {
			var matchHead = parsedMatches.a.a;
			var matchesTail = parsedMatches.b;
			var _v4 = matchHead.type_;
			if (_v4.$ === 'NormalType') {
				return A2($elm$core$List$cons, updtMatch, parsedMatches);
			} else {
				return _Utils_eq(matchModel.end, matchHead.start) ? A2($elm$core$List$cons, updtMatch, parsedMatches) : ((_Utils_cmp(matchModel.end, matchHead.start) < 0) ? A2(
					$elm$core$List$cons,
					updtMatch,
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(
							A3($elm$core$String$slice, matchModel.end, matchHead.start, rawText)),
						parsedMatches)) : parsedMatches);
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatches = F3(
	function (rawText, parsedMatches, matches) {
		parseTextMatches:
		while (true) {
			if (!matches.b) {
				if (!parsedMatches.b) {
					return $elm$core$String$isEmpty(rawText) ? _List_Nil : _List_fromArray(
						[
							$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(rawText)
						]);
				} else {
					var matchModel = parsedMatches.a.a;
					return (matchModel.start > 0) ? A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(
							A2($elm$core$String$left, matchModel.start, rawText)),
						parsedMatches) : parsedMatches;
				}
			} else {
				var match = matches.a;
				var matchesTail = matches.b;
				var $temp$rawText = rawText,
					$temp$parsedMatches = A3($dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatch, rawText, match, parsedMatches),
					$temp$matches = matchesTail;
				rawText = $temp$rawText;
				parsedMatches = $temp$parsedMatches;
				matches = $temp$matches;
				continue parseTextMatches;
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketLTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\<)'));
var $elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketOpen = {$: 'AngleBracketOpen'};
var $dillonkearns$elm_markdown$Markdown$Helpers$isEven = function (_int) {
	return !A2($elm$core$Basics$modBy, 2, _int);
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	if ((_v0.b && _v0.b.b) && (_v0.b.a.$ === 'Just')) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var delimiter = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
			{index: regMatch.index + backslashesLength, length: 1, meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketOpen}) : $elm$core$Maybe$Nothing;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$findAngleBracketLTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketLTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketRTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\>)'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketClose = function (a) {
	return {$: 'AngleBracketClose', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$Escaped = {$: 'Escaped'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$NotEscaped = {$: 'NotEscaped'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	if ((_v0.b && _v0.b.b) && (_v0.b.a.$ === 'Just')) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $elm$core$Maybe$Just(
			{
				index: regMatch.index + backslashesLength,
				length: 1,
				meaning: $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketClose($dillonkearns$elm_markdown$Markdown$InlineParser$NotEscaped) : $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketClose($dillonkearns$elm_markdown$Markdown$InlineParser$Escaped)
			});
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$findAngleBracketRTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketRTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)([^*])?(\\*+)([^*])?'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisToken = F2(
	function (a, b) {
		return {$: 'EmphasisToken', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isPunctuation = function (c) {
	switch (c.valueOf()) {
		case '!':
			return true;
		case '\"':
			return true;
		case '#':
			return true;
		case '%':
			return true;
		case '&':
			return true;
		case '\'':
			return true;
		case '(':
			return true;
		case ')':
			return true;
		case '*':
			return true;
		case ',':
			return true;
		case '-':
			return true;
		case '.':
			return true;
		case '/':
			return true;
		case ':':
			return true;
		case ';':
			return true;
		case '?':
			return true;
		case '@':
			return true;
		case '[':
			return true;
		case ']':
			return true;
		case '_':
			return true;
		case '{':
			return true;
		case '}':
			return true;
		case '~':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$containPunctuation = A2(
	$elm$core$String$foldl,
	F2(
		function (c, accum) {
			return accum || $dillonkearns$elm_markdown$Markdown$InlineParser$isPunctuation(c);
		}),
	false);
var $dillonkearns$elm_markdown$Markdown$InlineParser$isWhitespace = function (c) {
	switch (c.valueOf()) {
		case ' ':
			return true;
		case '\u000C':
			return true;
		case '\n':
			return true;
		case '\r':
			return true;
		case '\t':
			return true;
		case '\u000B':
			return true;
		case '\u00A0':
			return true;
		case '\u2028':
			return true;
		case '\u2029':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$containSpace = A2(
	$elm$core$String$foldl,
	F2(
		function (c, accum) {
			return accum || $dillonkearns$elm_markdown$Markdown$InlineParser$isWhitespace(c);
		}),
	false);
var $dillonkearns$elm_markdown$Markdown$InlineParser$getFringeRank = function (mstring) {
	if (mstring.$ === 'Just') {
		var string = mstring.a;
		return ($elm$core$String$isEmpty(string) || $dillonkearns$elm_markdown$Markdown$InlineParser$containSpace(string)) ? 0 : ($dillonkearns$elm_markdown$Markdown$InlineParser$containPunctuation(string) ? 1 : 2);
	} else {
		return 0;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken = F3(
	function (_char, rawText, regMatch) {
		var _v0 = regMatch.submatches;
		if ((((_v0.b && _v0.b.b) && _v0.b.b.b) && (_v0.b.b.a.$ === 'Just')) && _v0.b.b.b.b) {
			var maybeBackslashes = _v0.a;
			var _v1 = _v0.b;
			var maybeLeftFringe = _v1.a;
			var _v2 = _v1.b;
			var delimiter = _v2.a.a;
			var _v3 = _v2.b;
			var maybeRightFringe = _v3.a;
			var rFringeRank = $dillonkearns$elm_markdown$Markdown$InlineParser$getFringeRank(maybeRightFringe);
			var leftFringeLength = function () {
				if (maybeLeftFringe.$ === 'Just') {
					var left = maybeLeftFringe.a;
					return $elm$core$String$length(left);
				} else {
					return 0;
				}
			}();
			var mLeftFringe = ((!(!regMatch.index)) && (!leftFringeLength)) ? $elm$core$Maybe$Just(
				A3($elm$core$String$slice, regMatch.index - 1, regMatch.index, rawText)) : maybeLeftFringe;
			var backslashesLength = function () {
				if (maybeBackslashes.$ === 'Just') {
					var backslashes = maybeBackslashes.a;
					return $elm$core$String$length(backslashes);
				} else {
					return 0;
				}
			}();
			var isEscaped = ((!$dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) && (!leftFringeLength)) || function () {
				if ((mLeftFringe.$ === 'Just') && (mLeftFringe.a === '\\')) {
					return true;
				} else {
					return false;
				}
			}();
			var delimiterLength = isEscaped ? ($elm$core$String$length(delimiter) - 1) : $elm$core$String$length(delimiter);
			var lFringeRank = isEscaped ? 1 : $dillonkearns$elm_markdown$Markdown$InlineParser$getFringeRank(mLeftFringe);
			if ((delimiterLength <= 0) || (_Utils_eq(
				_char,
				_Utils_chr('_')) && ((lFringeRank === 2) && (rFringeRank === 2)))) {
				return $elm$core$Maybe$Nothing;
			} else {
				var index = ((regMatch.index + backslashesLength) + leftFringeLength) + (isEscaped ? 1 : 0);
				return $elm$core$Maybe$Just(
					{
						index: index,
						length: delimiterLength,
						meaning: A2(
							$dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisToken,
							_char,
							{leftFringeRank: lFringeRank, rightFringeRank: rFringeRank})
					});
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		A2(
			$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken,
			_Utils_chr('*'),
			str),
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$codeTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\`+)'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$CodeToken = function (a) {
	return {$: 'CodeToken', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToCodeToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	if ((_v0.b && _v0.b.b) && (_v0.b.a.$ === 'Just')) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backtick = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $elm$core$Maybe$Just(
			{
				index: regMatch.index + backslashesLength,
				length: $elm$core$String$length(backtick),
				meaning: $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $dillonkearns$elm_markdown$Markdown$InlineParser$CodeToken($dillonkearns$elm_markdown$Markdown$InlineParser$NotEscaped) : $dillonkearns$elm_markdown$Markdown$InlineParser$CodeToken($dillonkearns$elm_markdown$Markdown$InlineParser$Escaped)
			});
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$findCodeTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToCodeToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$codeTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$hardBreakTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(?:(\\\\+)|( {2,}))\\n'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken = {$: 'HardLineBreakToken'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	_v0$2:
	while (true) {
		if (_v0.b) {
			if (_v0.a.$ === 'Just') {
				var backslashes = _v0.a.a;
				var backslashesLength = $elm$core$String$length(backslashes);
				return (!$dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) ? $elm$core$Maybe$Just(
					{index: (regMatch.index + backslashesLength) - 1, length: 2, meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : $elm$core$Maybe$Nothing;
			} else {
				if (_v0.b.b && (_v0.b.a.$ === 'Just')) {
					var _v1 = _v0.b;
					return $elm$core$Maybe$Just(
						{
							index: regMatch.index,
							length: $elm$core$String$length(regMatch.match),
							meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken
						});
				} else {
					break _v0$2;
				}
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToSoftHardBreakToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	_v0$2:
	while (true) {
		if (_v0.b) {
			if (_v0.a.$ === 'Just') {
				var backslashes = _v0.a.a;
				var backslashesLength = $elm$core$String$length(backslashes);
				return $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
					{index: regMatch.index + backslashesLength, length: 1, meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : $elm$core$Maybe$Just(
					{index: (regMatch.index + backslashesLength) - 1, length: 2, meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken});
			} else {
				if (_v0.b.b) {
					var _v1 = _v0.b;
					var maybeSpaces = _v1.a;
					return $elm$core$Maybe$Just(
						{
							index: regMatch.index,
							length: $elm$core$String$length(regMatch.match),
							meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken
						});
				} else {
					break _v0$2;
				}
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$softAsHardLineBreak = false;
var $dillonkearns$elm_markdown$Markdown$InlineParser$softAsHardLineBreakTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(?:(\\\\+)|( *))\\n'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$findHardBreakTokens = function (str) {
	return $dillonkearns$elm_markdown$Markdown$InlineParser$softAsHardLineBreak ? A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToSoftHardBreakToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$softAsHardLineBreakTokenRegex, str)) : A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$hardBreakTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$linkImageCloseTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\])'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$SquareBracketClose = {$: 'SquareBracketClose'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	if ((_v0.b && _v0.b.b) && (_v0.b.a.$ === 'Just')) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
			{index: regMatch.index + backslashesLength, length: 1, meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$SquareBracketClose}) : $elm$core$Maybe$Nothing;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$findLinkImageCloseTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$linkImageCloseTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$linkImageOpenTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\!)?(\\[)'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$Active = {$: 'Active'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$ImageOpenToken = {$: 'ImageOpenToken'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken = function (a) {
	return {$: 'LinkOpenToken', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	if (((_v0.b && _v0.b.b) && _v0.b.b.b) && (_v0.b.b.a.$ === 'Just')) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var maybeImageOpen = _v1.a;
		var _v2 = _v1.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		var isEscaped = !$dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength);
		var index = isEscaped ? ((regMatch.index + backslashesLength) + 1) : (regMatch.index + backslashesLength);
		if (isEscaped) {
			if (maybeImageOpen.$ === 'Just') {
				return $elm$core$Maybe$Just(
					{
						index: index,
						length: 1,
						meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken($dillonkearns$elm_markdown$Markdown$InlineParser$Active)
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		} else {
			if (maybeImageOpen.$ === 'Just') {
				return $elm$core$Maybe$Just(
					{index: index, length: 2, meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$ImageOpenToken});
			} else {
				return $elm$core$Maybe$Just(
					{
						index: index,
						length: 1,
						meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken($dillonkearns$elm_markdown$Markdown$InlineParser$Active)
					});
			}
		}
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$findLinkImageOpenTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$linkImageOpenTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughToken = function (a) {
	return {$: 'StrikethroughToken', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToStrikethroughToken = function (regMatch) {
	var _v0 = regMatch.submatches;
	if ((_v0.b && _v0.b.b) && (_v0.b.a.$ === 'Just')) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var tilde = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		var _v2 = $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? _Utils_Tuple2(
			$elm$core$String$length(tilde),
			$dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughToken($dillonkearns$elm_markdown$Markdown$InlineParser$NotEscaped)) : _Utils_Tuple2(
			$elm$core$String$length(tilde),
			$dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughToken($dillonkearns$elm_markdown$Markdown$InlineParser$Escaped));
		var length = _v2.a;
		var meaning = _v2.b;
		return $elm$core$Maybe$Just(
			{index: regMatch.index + backslashesLength, length: length, meaning: meaning});
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$strikethroughTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(~{2,})([^~])?'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$findStrikethroughTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToStrikethroughToken,
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$strikethroughTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$underlineEmphasisTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)([^_])?(\\_+)([^_])?'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$findUnderlineEmphasisTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		A2(
			$dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken,
			_Utils_chr('_'),
			str),
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$underlineEmphasisTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex = F2(
	function (left, right) {
		if (left.b) {
			var lfirst = left.a;
			var lrest = left.b;
			if (right.b) {
				var rfirst = right.a;
				var rrest = right.b;
				return (_Utils_cmp(lfirst.index, rfirst.index) < 0) ? A2(
					$elm$core$List$cons,
					lfirst,
					A2($dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex, lrest, right)) : A2(
					$elm$core$List$cons,
					rfirst,
					A2($dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex, left, rrest));
			} else {
				return left;
			}
		} else {
			return right;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$tokenize = function (rawText) {
	return A2(
		$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
		$dillonkearns$elm_markdown$Markdown$InlineParser$findAngleBracketRTokens(rawText),
		A2(
			$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
			$dillonkearns$elm_markdown$Markdown$InlineParser$findAngleBracketLTokens(rawText),
			A2(
				$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
				$dillonkearns$elm_markdown$Markdown$InlineParser$findHardBreakTokens(rawText),
				A2(
					$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
					$dillonkearns$elm_markdown$Markdown$InlineParser$findLinkImageCloseTokens(rawText),
					A2(
						$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
						$dillonkearns$elm_markdown$Markdown$InlineParser$findLinkImageOpenTokens(rawText),
						A2(
							$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
							$dillonkearns$elm_markdown$Markdown$InlineParser$findStrikethroughTokens(rawText),
							A2(
								$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
								$dillonkearns$elm_markdown$Markdown$InlineParser$findUnderlineEmphasisTokens(rawText),
								A2(
									$dillonkearns$elm_markdown$Markdown$InlineParser$mergeByIndex,
									$dillonkearns$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens(rawText),
									$dillonkearns$elm_markdown$Markdown$InlineParser$findCodeTokens(rawText)))))))));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$CodeType = {$: 'CodeType'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisType = function (a) {
	return {$: 'EmphasisType', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$HtmlType = function (a) {
	return {$: 'HtmlType', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType = function (a) {
	return {$: 'ImageType', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$Inactive = {$: 'Inactive'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType = function (a) {
	return {$: 'LinkType', a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughType = {$: 'StrikethroughType'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$AutolinkType = function (a) {
	return {$: 'AutolinkType', a: a};
};
var $elm$regex$Regex$contains = _Regex_contains;
var $dillonkearns$elm_markdown$Markdown$InlineParser$decodeUrlRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('%(?:3B|2C|2F|3F|3A|40|26|3D|2B|24|23|25)'));
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $dillonkearns$elm_markdown$Markdown$InlineParser$encodeUrl = A2(
	$elm$core$Basics$composeR,
	$elm$url$Url$percentEncode,
	A2(
		$elm$regex$Regex$replace,
		$dillonkearns$elm_markdown$Markdown$InlineParser$decodeUrlRegex,
		function (match) {
			return A2(
				$elm$core$Maybe$withDefault,
				match.match,
				$elm$url$Url$percentDecode(match.match));
		}));
var $dillonkearns$elm_markdown$Markdown$InlineParser$urlRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^([A-Za-z][A-Za-z0-9.+\\-]{1,31}:[^<>\\x00-\\x20]*)$'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$autolinkToMatch = function (_v0) {
	var match = _v0.a;
	return A2($elm$regex$Regex$contains, $dillonkearns$elm_markdown$Markdown$InlineParser$urlRegex, match.text) ? $elm$core$Result$Ok(
		$dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				match,
				{
					type_: $dillonkearns$elm_markdown$Markdown$InlineParser$AutolinkType(
						_Utils_Tuple2(
							match.text,
							$dillonkearns$elm_markdown$Markdown$InlineParser$encodeUrl(match.text)))
				}))) : $elm$core$Result$Err(
		$dillonkearns$elm_markdown$Markdown$InlineParser$Match(match));
};
var $elm$regex$Regex$findAtMost = _Regex_findAtMost;
var $dillonkearns$elm_markdown$Markdown$Helpers$insideSquareBracketRegex = '[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*';
var $dillonkearns$elm_markdown$Markdown$InlineParser$refLabelRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\[\\s*(' + ($dillonkearns$elm_markdown$Markdown$Helpers$insideSquareBracketRegex + ')\\s*\\]')));
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Helpers$cleanWhitespaces = function (original) {
	return original;
};
var $dillonkearns$elm_markdown$Markdown$Helpers$prepareRefLabel = A2($elm$core$Basics$composeR, $dillonkearns$elm_markdown$Markdown$Helpers$cleanWhitespaces, $elm$core$String$toLower);
var $dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle = F2(
	function (rawUrl, maybeTitle) {
		return _Utils_Tuple2(
			$dillonkearns$elm_markdown$Markdown$InlineParser$encodeUrl(
				$dillonkearns$elm_markdown$Markdown$Helpers$formatStr(rawUrl)),
			A2($elm$core$Maybe$map, $dillonkearns$elm_markdown$Markdown$Helpers$formatStr, maybeTitle));
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$refRegexToMatch = F3(
	function (matchModel, references, maybeRegexMatch) {
		var refLabel = function (str) {
			return $elm$core$String$isEmpty(str) ? matchModel.text : str;
		}(
			A2(
				$elm$core$Maybe$withDefault,
				matchModel.text,
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Maybe$Nothing,
					A2(
						$elm$core$Maybe$andThen,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.submatches;
							},
							$elm$core$List$head),
						maybeRegexMatch))));
		var _v0 = A2(
			$elm$core$Dict$get,
			$dillonkearns$elm_markdown$Markdown$Helpers$prepareRefLabel(refLabel),
			references);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v1 = _v0.a;
			var rawUrl = _v1.a;
			var maybeTitle = _v1.b;
			var type_ = function () {
				var _v3 = matchModel.type_;
				if (_v3.$ === 'ImageType') {
					return $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType(
						A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle, rawUrl, maybeTitle));
				} else {
					return $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType(
						A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle, rawUrl, maybeTitle));
				}
			}();
			var regexMatchLength = function () {
				if (maybeRegexMatch.$ === 'Just') {
					var match = maybeRegexMatch.a.match;
					return $elm$core$String$length(match);
				} else {
					return 0;
				}
			}();
			return $elm$core$Maybe$Just(
				$dillonkearns$elm_markdown$Markdown$InlineParser$Match(
					_Utils_update(
						matchModel,
						{end: matchModel.end + regexMatchLength, type_: type_})));
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineReferences = F3(
	function (remainText, _v0, references) {
		var tempMatch = _v0.a;
		var matches = A3($elm$regex$Regex$findAtMost, 1, $dillonkearns$elm_markdown$Markdown$InlineParser$refLabelRegex, remainText);
		return A3(
			$dillonkearns$elm_markdown$Markdown$InlineParser$refRegexToMatch,
			tempMatch,
			references,
			$elm$core$List$head(matches));
	});
var $dillonkearns$elm_markdown$Markdown$Helpers$lineEndChars = '\\f\\v\\r\\n';
var $dillonkearns$elm_markdown$Markdown$Helpers$whiteSpaceChars = ' \\t\\f\\v\\r\\n';
var $dillonkearns$elm_markdown$Markdown$InlineParser$hrefRegex = '(?:<([^<>' + ($dillonkearns$elm_markdown$Markdown$Helpers$lineEndChars + (']*)>|([^' + ($dillonkearns$elm_markdown$Markdown$Helpers$whiteSpaceChars + ('\\(\\)\\\\]*(?:\\\\.[^' + ($dillonkearns$elm_markdown$Markdown$Helpers$whiteSpaceChars + '\\(\\)\\\\]*)*))')))));
var $dillonkearns$elm_markdown$Markdown$Helpers$titleRegex = '(?:[' + ($dillonkearns$elm_markdown$Markdown$Helpers$whiteSpaceChars + (']+' + ('(?:\'([^\'\\\\]*(?:\\\\.[^\'\\\\]*)*)\'|' + ('\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|' + '\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?'))));
var $dillonkearns$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\(\\s*' + ($dillonkearns$elm_markdown$Markdown$InlineParser$hrefRegex + ($dillonkearns$elm_markdown$Markdown$Helpers$titleRegex + '\\s*\\)'))));
var $dillonkearns$elm_markdown$Markdown$Helpers$returnFirstJust = function (maybes) {
	var process = F2(
		function (a, maybeFound) {
			if (maybeFound.$ === 'Just') {
				var found = maybeFound.a;
				return $elm$core$Maybe$Just(found);
			} else {
				return a;
			}
		});
	return A3($elm$core$List$foldl, process, $elm$core$Maybe$Nothing, maybes);
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch = F2(
	function (matchModel, regexMatch) {
		var _v0 = regexMatch.submatches;
		if ((((_v0.b && _v0.b.b) && _v0.b.b.b) && _v0.b.b.b.b) && _v0.b.b.b.b.b) {
			var maybeRawUrlAngleBrackets = _v0.a;
			var _v1 = _v0.b;
			var maybeRawUrlWithoutBrackets = _v1.a;
			var _v2 = _v1.b;
			var maybeTitleSingleQuotes = _v2.a;
			var _v3 = _v2.b;
			var maybeTitleDoubleQuotes = _v3.a;
			var _v4 = _v3.b;
			var maybeTitleParenthesis = _v4.a;
			var maybeTitle = $dillonkearns$elm_markdown$Markdown$Helpers$returnFirstJust(
				_List_fromArray(
					[maybeTitleSingleQuotes, maybeTitleDoubleQuotes, maybeTitleParenthesis]));
			var toMatch = function (rawUrl) {
				return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
					_Utils_update(
						matchModel,
						{
							end: matchModel.end + $elm$core$String$length(regexMatch.match),
							type_: function () {
								var _v5 = matchModel.type_;
								if (_v5.$ === 'ImageType') {
									return $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType;
								} else {
									return $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType;
								}
							}()(
								A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle, rawUrl, maybeTitle))
						}));
			};
			var maybeRawUrl = $dillonkearns$elm_markdown$Markdown$Helpers$returnFirstJust(
				_List_fromArray(
					[maybeRawUrlAngleBrackets, maybeRawUrlWithoutBrackets]));
			return $elm$core$Maybe$Just(
				toMatch(
					A2($elm$core$Maybe$withDefault, '', maybeRawUrl)));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType = F3(
	function (remainText, _v0, refs) {
		var tempMatch = _v0.a;
		var _v1 = A3($elm$regex$Regex$findAtMost, 1, $dillonkearns$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex, remainText);
		if (_v1.b) {
			var first = _v1.a;
			var _v2 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch, tempMatch, first);
			if (_v2.$ === 'Just') {
				var match = _v2.a;
				return $elm$core$Maybe$Just(match);
			} else {
				return A3(
					$dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineReferences,
					remainText,
					$dillonkearns$elm_markdown$Markdown$InlineParser$Match(tempMatch),
					refs);
			}
		} else {
			return A3(
				$dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineReferences,
				remainText,
				$dillonkearns$elm_markdown$Markdown$InlineParser$Match(tempMatch),
				refs);
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping = F2(
	function (_v0, remainMatches) {
		var match = _v0.a;
		var overlappingMatches = $elm$core$List$filter(
			function (_v1) {
				var testMatch = _v1.a;
				return (_Utils_cmp(match.end, testMatch.start) > 0) && (_Utils_cmp(match.end, testMatch.end) < 0);
			});
		return ($elm$core$List$isEmpty(remainMatches) || $elm$core$List$isEmpty(
			overlappingMatches(remainMatches))) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$List$cons,
				$dillonkearns$elm_markdown$Markdown$InlineParser$Match(match),
				remainMatches)) : $elm$core$Maybe$Nothing;
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$emailRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^([a-zA-Z0-9.!#$%&\'*+\\/=?^_`{|}~\\-]+@[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*)$'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch = function (_v0) {
	var match = _v0.a;
	return A2($elm$regex$Regex$contains, $dillonkearns$elm_markdown$Markdown$InlineParser$emailRegex, match.text) ? $elm$core$Result$Ok(
		$dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				match,
				{
					type_: $dillonkearns$elm_markdown$Markdown$InlineParser$AutolinkType(
						_Utils_Tuple2(
							match.text,
							'mailto:' + $dillonkearns$elm_markdown$Markdown$InlineParser$encodeUrl(match.text)))
				}))) : $elm$core$Result$Err(
		$dillonkearns$elm_markdown$Markdown$InlineParser$Match(match));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$findTokenHelp = F3(
	function (innerTokens, isToken, tokens) {
		findTokenHelp:
		while (true) {
			if (!tokens.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var nextToken = tokens.a;
				var remainingTokens = tokens.b;
				if (isToken(nextToken)) {
					return $elm$core$Maybe$Just(
						_Utils_Tuple3(
							nextToken,
							$elm$core$List$reverse(innerTokens),
							remainingTokens));
				} else {
					var $temp$innerTokens = A2($elm$core$List$cons, nextToken, innerTokens),
						$temp$isToken = isToken,
						$temp$tokens = remainingTokens;
					innerTokens = $temp$innerTokens;
					isToken = $temp$isToken;
					tokens = $temp$tokens;
					continue findTokenHelp;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$findToken = F2(
	function (isToken, tokens) {
		return A3($dillonkearns$elm_markdown$Markdown$InlineParser$findTokenHelp, _List_Nil, isToken, tokens);
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$HtmlToken = F2(
	function (a, b) {
		return {$: 'HtmlToken', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$NotOpening = {$: 'NotOpening'};
var $elm$parser$Parser$Advanced$getOffset = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.offset, s);
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$htmlToToken = F2(
	function (rawText, _v0) {
		var match = _v0.a;
		var consumedCharacters = A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$keeper,
					$elm$parser$Parser$Advanced$succeed(
						F3(
							function (startOffset, htmlTag, endOffset) {
								return {htmlTag: htmlTag, length: endOffset - startOffset};
							})),
					$elm$parser$Parser$Advanced$getOffset),
				$dillonkearns$elm_markdown$HtmlParser$html),
			$elm$parser$Parser$Advanced$getOffset);
		var parsed = A2(
			$elm$parser$Parser$Advanced$run,
			consumedCharacters,
			A2($elm$core$String$dropLeft, match.start, rawText));
		if (parsed.$ === 'Ok') {
			var htmlTag = parsed.a.htmlTag;
			var length = parsed.a.length;
			var htmlToken = A2($dillonkearns$elm_markdown$Markdown$InlineParser$HtmlToken, $dillonkearns$elm_markdown$Markdown$InlineParser$NotOpening, htmlTag);
			return $elm$core$Maybe$Just(
				{index: match.start, length: length, meaning: htmlToken});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Helpers$ifError = F2(
	function (_function, result) {
		if (result.$ === 'Ok') {
			return result;
		} else {
			var err = result.a;
			return _function(err);
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isCloseToken = F2(
	function (htmlModel, token) {
		return false;
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isCodeTokenPair = F2(
	function (closeToken, openToken) {
		var _v0 = openToken.meaning;
		if (_v0.$ === 'CodeToken') {
			if (_v0.a.$ === 'Escaped') {
				var _v1 = _v0.a;
				return _Utils_eq(openToken.length - 1, closeToken.length);
			} else {
				var _v2 = _v0.a;
				return _Utils_eq(openToken.length, closeToken.length);
			}
		} else {
			return false;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken = function (token) {
	var _v0 = token.meaning;
	switch (_v0.$) {
		case 'LinkOpenToken':
			return true;
		case 'ImageOpenToken':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken = F2(
	function (closeToken, openToken) {
		var _v0 = openToken.meaning;
		if (_v0.$ === 'EmphasisToken') {
			var openChar = _v0.a;
			var open = _v0.b;
			var _v1 = closeToken.meaning;
			if (_v1.$ === 'EmphasisToken') {
				var closeChar = _v1.a;
				var close = _v1.b;
				return _Utils_eq(openChar, closeChar) ? ((_Utils_eq(open.leftFringeRank, open.rightFringeRank) || _Utils_eq(close.leftFringeRank, close.rightFringeRank)) ? (!(!A2($elm$core$Basics$modBy, 3, closeToken.length + openToken.length))) : true) : false;
			} else {
				return false;
			}
		} else {
			return false;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isStrikethroughTokenPair = F2(
	function (closeToken, openToken) {
		var _v0 = function () {
			var _v1 = openToken.meaning;
			if (_v1.$ === 'StrikethroughToken') {
				if (_v1.a.$ === 'Escaped') {
					var _v2 = _v1.a;
					return _Utils_Tuple2(true, openToken.length - 1);
				} else {
					var _v3 = _v1.a;
					return _Utils_Tuple2(true, openToken.length);
				}
			} else {
				return _Utils_Tuple2(false, 0);
			}
		}();
		var openTokenIsStrikethrough = _v0.a;
		var openTokenLength = _v0.b;
		var _v4 = function () {
			var _v5 = closeToken.meaning;
			if (_v5.$ === 'StrikethroughToken') {
				if (_v5.a.$ === 'Escaped') {
					var _v6 = _v5.a;
					return _Utils_Tuple2(true, closeToken.length - 1);
				} else {
					var _v7 = _v5.a;
					return _Utils_Tuple2(true, closeToken.length);
				}
			} else {
				return _Utils_Tuple2(false, 0);
			}
		}();
		var closeTokenIsStrikethrough = _v4.a;
		var closeTokenLength = _v4.b;
		return closeTokenIsStrikethrough && (openTokenIsStrikethrough && _Utils_eq(closeTokenLength, openTokenLength));
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakType = {$: 'HardLineBreakType'};
var $dillonkearns$elm_markdown$Markdown$InlineParser$tokenToMatch = F2(
	function (token, type_) {
		return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			{end: token.index + token.length, matches: _List_Nil, start: token.index, text: '', textEnd: 0, textStart: 0, type_: type_});
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$lineBreakTTM = F5(
	function (remaining, tokens, matches, refs, rawText) {
		lineBreakTTM:
		while (true) {
			if (!remaining.b) {
				return matches;
			} else {
				var token = remaining.a;
				var tokensTail = remaining.b;
				var _v1 = token.meaning;
				if (_v1.$ === 'HardLineBreakToken') {
					var $temp$remaining = tokensTail,
						$temp$tokens = tokens,
						$temp$matches = A2(
						$elm$core$List$cons,
						A2($dillonkearns$elm_markdown$Markdown$InlineParser$tokenToMatch, token, $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakType),
						matches),
						$temp$refs = refs,
						$temp$rawText = rawText;
					remaining = $temp$remaining;
					tokens = $temp$tokens;
					matches = $temp$matches;
					refs = $temp$refs;
					rawText = $temp$rawText;
					continue lineBreakTTM;
				} else {
					var $temp$remaining = tokensTail,
						$temp$tokens = A2($elm$core$List$cons, token, tokens),
						$temp$matches = matches,
						$temp$refs = refs,
						$temp$rawText = rawText;
					remaining = $temp$remaining;
					tokens = $temp$tokens;
					matches = $temp$matches;
					refs = $temp$refs;
					rawText = $temp$rawText;
					continue lineBreakTTM;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens = F2(
	function (_v0, tokensTail) {
		var match = _v0.a;
		return A2(
			$elm$core$List$filter,
			function (token) {
				return _Utils_cmp(token.index, match.end) > -1;
			},
			tokensTail);
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketsToMatch = F6(
	function (closeToken, escaped, matches, references, rawText, _v46) {
		var openToken = _v46.a;
		var remainTokens = _v46.c;
		var result = A2(
			$dillonkearns$elm_markdown$Markdown$Helpers$ifError,
			$dillonkearns$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch,
			$dillonkearns$elm_markdown$Markdown$InlineParser$autolinkToMatch(
				A7(
					$dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
					references,
					rawText,
					function (s) {
						return s;
					},
					$dillonkearns$elm_markdown$Markdown$InlineParser$CodeType,
					openToken,
					closeToken,
					_List_Nil)));
		if (result.$ === 'Err') {
			var tempMatch = result.a;
			if (escaped.$ === 'NotEscaped') {
				var _v49 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$htmlToToken, rawText, tempMatch);
				if (_v49.$ === 'Just') {
					var newToken = _v49.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							A2($elm$core$List$cons, newToken, remainTokens),
							matches));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			} else {
				return $elm$core$Maybe$Nothing;
			}
		} else {
			var newMatch = result.a;
			return $elm$core$Maybe$Just(
				_Utils_Tuple2(
					remainTokens,
					A2($elm$core$List$cons, newMatch, matches)));
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM = F5(
	function (remaining, tokens, matches, references, rawText) {
		codeAutolinkTypeHtmlTagTTM:
		while (true) {
			if (!remaining.b) {
				return A5(
					$dillonkearns$elm_markdown$Markdown$InlineParser$htmlElementTTM,
					$elm$core$List$reverse(tokens),
					_List_Nil,
					matches,
					references,
					rawText);
			} else {
				var token = remaining.a;
				var tokensTail = remaining.b;
				var _v38 = token.meaning;
				switch (_v38.$) {
					case 'CodeToken':
						var isEscaped = _v38.a;
						var _v39 = A2(
							$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
							$dillonkearns$elm_markdown$Markdown$InlineParser$isCodeTokenPair(token),
							tokens);
						if (_v39.$ === 'Just') {
							var code = _v39.a;
							var _v40 = A5($dillonkearns$elm_markdown$Markdown$InlineParser$codeToMatch, token, matches, references, rawText, code);
							var newTokens = _v40.a;
							var newMatches = _v40.b;
							var $temp$remaining = tokensTail,
								$temp$tokens = newTokens,
								$temp$matches = newMatches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue codeAutolinkTypeHtmlTagTTM;
						} else {
							var $temp$remaining = tokensTail,
								$temp$tokens = A2($elm$core$List$cons, token, tokens),
								$temp$matches = matches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue codeAutolinkTypeHtmlTagTTM;
						}
					case 'AngleBracketClose':
						var isEscaped = _v38.a;
						var isAngleBracketOpen = function (_v45) {
							var meaning = _v45.meaning;
							if (meaning.$ === 'AngleBracketOpen') {
								return true;
							} else {
								return false;
							}
						};
						var _v41 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$findToken, isAngleBracketOpen, tokens);
						if (_v41.$ === 'Just') {
							var found = _v41.a;
							var _v42 = A6($dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketsToMatch, token, isEscaped, matches, references, rawText, found);
							if (_v42.$ === 'Just') {
								var _v43 = _v42.a;
								var newTokens = _v43.a;
								var newMatches = _v43.b;
								var $temp$remaining = tokensTail,
									$temp$tokens = A2(
									$elm$core$List$filter,
									A2($elm$core$Basics$composeL, $elm$core$Basics$not, isAngleBracketOpen),
									newTokens),
									$temp$matches = newMatches,
									$temp$references = references,
									$temp$rawText = rawText;
								remaining = $temp$remaining;
								tokens = $temp$tokens;
								matches = $temp$matches;
								references = $temp$references;
								rawText = $temp$rawText;
								continue codeAutolinkTypeHtmlTagTTM;
							} else {
								var $temp$remaining = tokensTail,
									$temp$tokens = A2(
									$elm$core$List$filter,
									A2($elm$core$Basics$composeL, $elm$core$Basics$not, isAngleBracketOpen),
									tokens),
									$temp$matches = matches,
									$temp$references = references,
									$temp$rawText = rawText;
								remaining = $temp$remaining;
								tokens = $temp$tokens;
								matches = $temp$matches;
								references = $temp$references;
								rawText = $temp$rawText;
								continue codeAutolinkTypeHtmlTagTTM;
							}
						} else {
							var $temp$remaining = tokensTail,
								$temp$tokens = A2(
								$elm$core$List$filter,
								A2($elm$core$Basics$composeL, $elm$core$Basics$not, isAngleBracketOpen),
								tokens),
								$temp$matches = matches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue codeAutolinkTypeHtmlTagTTM;
						}
					default:
						var $temp$remaining = tokensTail,
							$temp$tokens = A2($elm$core$List$cons, token, tokens),
							$temp$matches = matches,
							$temp$references = references,
							$temp$rawText = rawText;
						remaining = $temp$remaining;
						tokens = $temp$tokens;
						matches = $temp$matches;
						references = $temp$references;
						rawText = $temp$rawText;
						continue codeAutolinkTypeHtmlTagTTM;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$codeToMatch = F5(
	function (closeToken, matches, references, rawText, _v34) {
		var openToken = _v34.a;
		var remainTokens = _v34.c;
		var updatedOpenToken = function () {
			var _v35 = openToken.meaning;
			if ((_v35.$ === 'CodeToken') && (_v35.a.$ === 'Escaped')) {
				var _v36 = _v35.a;
				return _Utils_update(
					openToken,
					{index: openToken.index + 1, length: openToken.length - 1});
			} else {
				return openToken;
			}
		}();
		var match = A7($dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch, references, rawText, $dillonkearns$elm_markdown$Markdown$Helpers$cleanWhitespaces, $dillonkearns$elm_markdown$Markdown$InlineParser$CodeType, updatedOpenToken, closeToken, _List_Nil);
		return _Utils_Tuple2(
			remainTokens,
			A2($elm$core$List$cons, match, matches));
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$emphasisTTM = F5(
	function (remaining, tokens, matches, references, rawText) {
		emphasisTTM:
		while (true) {
			if (!remaining.b) {
				return A5(
					$dillonkearns$elm_markdown$Markdown$InlineParser$strikethroughTTM,
					$elm$core$List$reverse(tokens),
					_List_Nil,
					matches,
					references,
					rawText);
			} else {
				var token = remaining.a;
				var tokensTail = remaining.b;
				var _v29 = token.meaning;
				if (_v29.$ === 'EmphasisToken') {
					var _char = _v29.a;
					var leftFringeRank = _v29.b.leftFringeRank;
					var rightFringeRank = _v29.b.rightFringeRank;
					if (_Utils_eq(leftFringeRank, rightFringeRank)) {
						if ((!(!rightFringeRank)) && ((!_Utils_eq(
							_char,
							_Utils_chr('_'))) || (rightFringeRank === 1))) {
							var _v30 = A2(
								$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
								$dillonkearns$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
								tokens);
							if (_v30.$ === 'Just') {
								var found = _v30.a;
								var _v31 = A5($dillonkearns$elm_markdown$Markdown$InlineParser$emphasisToMatch, references, rawText, token, tokensTail, found);
								var newRemaining = _v31.a;
								var match = _v31.b;
								var newTokens = _v31.c;
								var $temp$remaining = newRemaining,
									$temp$tokens = newTokens,
									$temp$matches = A2($elm$core$List$cons, match, matches),
									$temp$references = references,
									$temp$rawText = rawText;
								remaining = $temp$remaining;
								tokens = $temp$tokens;
								matches = $temp$matches;
								references = $temp$references;
								rawText = $temp$rawText;
								continue emphasisTTM;
							} else {
								var $temp$remaining = tokensTail,
									$temp$tokens = A2($elm$core$List$cons, token, tokens),
									$temp$matches = matches,
									$temp$references = references,
									$temp$rawText = rawText;
								remaining = $temp$remaining;
								tokens = $temp$tokens;
								matches = $temp$matches;
								references = $temp$references;
								rawText = $temp$rawText;
								continue emphasisTTM;
							}
						} else {
							var $temp$remaining = tokensTail,
								$temp$tokens = tokens,
								$temp$matches = matches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue emphasisTTM;
						}
					} else {
						if (_Utils_cmp(leftFringeRank, rightFringeRank) < 0) {
							var $temp$remaining = tokensTail,
								$temp$tokens = A2($elm$core$List$cons, token, tokens),
								$temp$matches = matches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue emphasisTTM;
						} else {
							var _v32 = A2(
								$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
								$dillonkearns$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
								tokens);
							if (_v32.$ === 'Just') {
								var found = _v32.a;
								var _v33 = A5($dillonkearns$elm_markdown$Markdown$InlineParser$emphasisToMatch, references, rawText, token, tokensTail, found);
								var newRemaining = _v33.a;
								var match = _v33.b;
								var newTokens = _v33.c;
								var $temp$remaining = newRemaining,
									$temp$tokens = newTokens,
									$temp$matches = A2($elm$core$List$cons, match, matches),
									$temp$references = references,
									$temp$rawText = rawText;
								remaining = $temp$remaining;
								tokens = $temp$tokens;
								matches = $temp$matches;
								references = $temp$references;
								rawText = $temp$rawText;
								continue emphasisTTM;
							} else {
								var $temp$remaining = tokensTail,
									$temp$tokens = tokens,
									$temp$matches = matches,
									$temp$references = references,
									$temp$rawText = rawText;
								remaining = $temp$remaining;
								tokens = $temp$tokens;
								matches = $temp$matches;
								references = $temp$references;
								rawText = $temp$rawText;
								continue emphasisTTM;
							}
						}
					}
				} else {
					var $temp$remaining = tokensTail,
						$temp$tokens = A2($elm$core$List$cons, token, tokens),
						$temp$matches = matches,
						$temp$references = references,
						$temp$rawText = rawText;
					remaining = $temp$remaining;
					tokens = $temp$tokens;
					matches = $temp$matches;
					references = $temp$references;
					rawText = $temp$rawText;
					continue emphasisTTM;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$emphasisToMatch = F5(
	function (references, rawText, closeToken, tokensTail, _v27) {
		var openToken = _v27.a;
		var innerTokens = _v27.b;
		var remainTokens = _v27.c;
		var remainLength = openToken.length - closeToken.length;
		var updt = (!remainLength) ? {closeToken: closeToken, openToken: openToken, remainTokens: remainTokens, tokensTail: tokensTail} : ((remainLength > 0) ? {
			closeToken: closeToken,
			openToken: _Utils_update(
				openToken,
				{index: openToken.index + remainLength, length: closeToken.length}),
			remainTokens: A2(
				$elm$core$List$cons,
				_Utils_update(
					openToken,
					{length: remainLength}),
				remainTokens),
			tokensTail: tokensTail
		} : {
			closeToken: _Utils_update(
				closeToken,
				{length: openToken.length}),
			openToken: openToken,
			remainTokens: remainTokens,
			tokensTail: A2(
				$elm$core$List$cons,
				_Utils_update(
					closeToken,
					{index: closeToken.index + openToken.length, length: -remainLength}),
				tokensTail)
		});
		var match = A7(
			$dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
			references,
			rawText,
			function (s) {
				return s;
			},
			$dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisType(updt.openToken.length),
			updt.openToken,
			updt.closeToken,
			$elm$core$List$reverse(innerTokens));
		return _Utils_Tuple3(updt.tokensTail, match, updt.remainTokens);
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$htmlElementTTM = F5(
	function (remaining, tokens, matches, references, rawText) {
		htmlElementTTM:
		while (true) {
			if (!remaining.b) {
				return A5(
					$dillonkearns$elm_markdown$Markdown$InlineParser$linkImageTypeTTM,
					$elm$core$List$reverse(tokens),
					_List_Nil,
					matches,
					references,
					rawText);
			} else {
				var token = remaining.a;
				var tokensTail = remaining.b;
				var _v23 = token.meaning;
				if (_v23.$ === 'HtmlToken') {
					var isOpen = _v23.a;
					var htmlModel = _v23.b;
					if (isOpen.$ === 'NotOpening') {
						var $temp$remaining = tokensTail,
							$temp$tokens = tokens,
							$temp$matches = A2(
							$elm$core$List$cons,
							A2(
								$dillonkearns$elm_markdown$Markdown$InlineParser$tokenToMatch,
								token,
								$dillonkearns$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel)),
							matches),
							$temp$references = references,
							$temp$rawText = rawText;
						remaining = $temp$remaining;
						tokens = $temp$tokens;
						matches = $temp$matches;
						references = $temp$references;
						rawText = $temp$rawText;
						continue htmlElementTTM;
					} else {
						var _v25 = A2(
							$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
							$dillonkearns$elm_markdown$Markdown$InlineParser$isCloseToken(htmlModel),
							tokensTail);
						if (_v25.$ === 'Nothing') {
							var $temp$remaining = tokensTail,
								$temp$tokens = tokens,
								$temp$matches = A2(
								$elm$core$List$cons,
								A2(
									$dillonkearns$elm_markdown$Markdown$InlineParser$tokenToMatch,
									token,
									$dillonkearns$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel)),
								matches),
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue htmlElementTTM;
						} else {
							var _v26 = _v25.a;
							var closeToken = _v26.a;
							var innerTokens = _v26.b;
							var newTail = _v26.c;
							var newMatch = A7(
								$dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
								references,
								rawText,
								function (s) {
									return s;
								},
								$dillonkearns$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel),
								token,
								closeToken,
								innerTokens);
							var $temp$remaining = newTail,
								$temp$tokens = tokens,
								$temp$matches = A2($elm$core$List$cons, newMatch, matches),
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue htmlElementTTM;
						}
					}
				} else {
					var $temp$remaining = tokensTail,
						$temp$tokens = A2($elm$core$List$cons, token, tokens),
						$temp$matches = matches,
						$temp$references = references,
						$temp$rawText = rawText;
					remaining = $temp$remaining;
					tokens = $temp$tokens;
					matches = $temp$matches;
					references = $temp$references;
					rawText = $temp$rawText;
					continue htmlElementTTM;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$linkImageTypeTTM = F5(
	function (remaining, tokens, matches, references, rawText) {
		linkImageTypeTTM:
		while (true) {
			if (!remaining.b) {
				return A5(
					$dillonkearns$elm_markdown$Markdown$InlineParser$emphasisTTM,
					$elm$core$List$reverse(tokens),
					_List_Nil,
					matches,
					references,
					rawText);
			} else {
				var token = remaining.a;
				var tokensTail = remaining.b;
				var _v18 = token.meaning;
				if (_v18.$ === 'SquareBracketClose') {
					var _v19 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$findToken, $dillonkearns$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken, tokens);
					if (_v19.$ === 'Just') {
						var found = _v19.a;
						var _v20 = A6($dillonkearns$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch, token, tokensTail, matches, references, rawText, found);
						if (_v20.$ === 'Just') {
							var _v21 = _v20.a;
							var x = _v21.a;
							var newMatches = _v21.b;
							var newTokens = _v21.c;
							var $temp$remaining = x,
								$temp$tokens = newTokens,
								$temp$matches = newMatches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue linkImageTypeTTM;
						} else {
							var $temp$remaining = tokensTail,
								$temp$tokens = tokens,
								$temp$matches = matches,
								$temp$references = references,
								$temp$rawText = rawText;
							remaining = $temp$remaining;
							tokens = $temp$tokens;
							matches = $temp$matches;
							references = $temp$references;
							rawText = $temp$rawText;
							continue linkImageTypeTTM;
						}
					} else {
						var $temp$remaining = tokensTail,
							$temp$tokens = tokens,
							$temp$matches = matches,
							$temp$references = references,
							$temp$rawText = rawText;
						remaining = $temp$remaining;
						tokens = $temp$tokens;
						matches = $temp$matches;
						references = $temp$references;
						rawText = $temp$rawText;
						continue linkImageTypeTTM;
					}
				} else {
					var $temp$remaining = tokensTail,
						$temp$tokens = A2($elm$core$List$cons, token, tokens),
						$temp$matches = matches,
						$temp$references = references,
						$temp$rawText = rawText;
					remaining = $temp$remaining;
					tokens = $temp$tokens;
					matches = $temp$matches;
					references = $temp$references;
					rawText = $temp$rawText;
					continue linkImageTypeTTM;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch = F6(
	function (closeToken, tokensTail, oldMatches, references, rawText, _v8) {
		var openToken = _v8.a;
		var innerTokens = _v8.b;
		var remainTokens = _v8.c;
		var removeOpenToken = _Utils_Tuple3(
			tokensTail,
			oldMatches,
			_Utils_ap(innerTokens, remainTokens));
		var remainText = A2($elm$core$String$dropLeft, closeToken.index + 1, rawText);
		var inactivateLinkOpenToken = function (token) {
			var _v16 = token.meaning;
			if (_v16.$ === 'LinkOpenToken') {
				return _Utils_update(
					token,
					{
						meaning: $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken($dillonkearns$elm_markdown$Markdown$InlineParser$Inactive)
					});
			} else {
				return token;
			}
		};
		var findTempMatch = function (isLinkType) {
			return A7(
				$dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
				references,
				rawText,
				function (s) {
					return s;
				},
				isLinkType ? $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType(
					_Utils_Tuple2('', $elm$core$Maybe$Nothing)) : $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType(
					_Utils_Tuple2('', $elm$core$Maybe$Nothing)),
				openToken,
				closeToken,
				$elm$core$List$reverse(innerTokens));
		};
		var _v9 = openToken.meaning;
		switch (_v9.$) {
			case 'ImageOpenToken':
				var tempMatch = findTempMatch(false);
				var _v10 = A3($dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType, remainText, tempMatch, references);
				if (_v10.$ === 'Nothing') {
					return $elm$core$Maybe$Just(removeOpenToken);
				} else {
					var match = _v10.a;
					var _v11 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping, match, oldMatches);
					if (_v11.$ === 'Just') {
						var matches = _v11.a;
						return $elm$core$Maybe$Just(
							_Utils_Tuple3(
								A2($dillonkearns$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens, match, tokensTail),
								matches,
								remainTokens));
					} else {
						return $elm$core$Maybe$Just(removeOpenToken);
					}
				}
			case 'LinkOpenToken':
				if (_v9.a.$ === 'Active') {
					var _v12 = _v9.a;
					var tempMatch = findTempMatch(true);
					var _v13 = A3($dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType, remainText, tempMatch, references);
					if (_v13.$ === 'Nothing') {
						return $elm$core$Maybe$Just(removeOpenToken);
					} else {
						var match = _v13.a;
						var _v14 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping, match, oldMatches);
						if (_v14.$ === 'Just') {
							var matches = _v14.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple3(
									A2($dillonkearns$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens, match, tokensTail),
									matches,
									A2($elm$core$List$map, inactivateLinkOpenToken, remainTokens)));
						} else {
							return $elm$core$Maybe$Just(removeOpenToken);
						}
					}
				} else {
					var _v15 = _v9.a;
					return $elm$core$Maybe$Just(removeOpenToken);
				}
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$strikethroughTTM = F5(
	function (remaining, tokens, matches, references, rawText) {
		strikethroughTTM:
		while (true) {
			if (!remaining.b) {
				return A5(
					$dillonkearns$elm_markdown$Markdown$InlineParser$lineBreakTTM,
					$elm$core$List$reverse(tokens),
					_List_Nil,
					matches,
					references,
					rawText);
			} else {
				var token = remaining.a;
				var tokensTail = remaining.b;
				var _v5 = token.meaning;
				if (_v5.$ === 'StrikethroughToken') {
					var isEscaped = _v5.a;
					var _v6 = A2(
						$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
						$dillonkearns$elm_markdown$Markdown$InlineParser$isStrikethroughTokenPair(token),
						tokens);
					if (_v6.$ === 'Just') {
						var content = _v6.a;
						var _v7 = A5($dillonkearns$elm_markdown$Markdown$InlineParser$strikethroughToMatch, token, matches, references, rawText, content);
						var newTokens = _v7.a;
						var newMatches = _v7.b;
						var $temp$remaining = tokensTail,
							$temp$tokens = newTokens,
							$temp$matches = newMatches,
							$temp$references = references,
							$temp$rawText = rawText;
						remaining = $temp$remaining;
						tokens = $temp$tokens;
						matches = $temp$matches;
						references = $temp$references;
						rawText = $temp$rawText;
						continue strikethroughTTM;
					} else {
						var $temp$remaining = tokensTail,
							$temp$tokens = A2($elm$core$List$cons, token, tokens),
							$temp$matches = matches,
							$temp$references = references,
							$temp$rawText = rawText;
						remaining = $temp$remaining;
						tokens = $temp$tokens;
						matches = $temp$matches;
						references = $temp$references;
						rawText = $temp$rawText;
						continue strikethroughTTM;
					}
				} else {
					var $temp$remaining = tokensTail,
						$temp$tokens = A2($elm$core$List$cons, token, tokens),
						$temp$matches = matches,
						$temp$references = references,
						$temp$rawText = rawText;
					remaining = $temp$remaining;
					tokens = $temp$tokens;
					matches = $temp$matches;
					references = $temp$references;
					rawText = $temp$rawText;
					continue strikethroughTTM;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$strikethroughToMatch = F5(
	function (closeToken, matches, references, rawText, _v1) {
		var openToken = _v1.a;
		var remainTokens = _v1.c;
		var updatedOpenToken = function () {
			var _v2 = openToken.meaning;
			if ((_v2.$ === 'StrikethroughToken') && (_v2.a.$ === 'Escaped')) {
				var _v3 = _v2.a;
				return _Utils_update(
					openToken,
					{index: openToken.index + 1, length: openToken.length - 1});
			} else {
				return openToken;
			}
		}();
		var match = A7($dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch, references, rawText, $dillonkearns$elm_markdown$Markdown$Helpers$cleanWhitespaces, $dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughType, updatedOpenToken, closeToken, _List_Nil);
		return _Utils_Tuple2(
			remainTokens,
			A2($elm$core$List$cons, match, matches));
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch = F7(
	function (references, rawText, processText, type_, openToken, closeToken, innerTokens) {
		var textStart = openToken.index + openToken.length;
		var textEnd = closeToken.index;
		var text = processText(
			A3($elm$core$String$slice, textStart, textEnd, rawText));
		var start = openToken.index;
		var end = closeToken.index + closeToken.length;
		var match = {end: end, matches: _List_Nil, start: start, text: text, textEnd: textEnd, textStart: textStart, type_: type_};
		var matches = A2(
			$elm$core$List$map,
			function (_v0) {
				var matchModel = _v0.a;
				return A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareChildMatch, match, matchModel);
			},
			A4($dillonkearns$elm_markdown$Markdown$InlineParser$tokensToMatches, innerTokens, _List_Nil, references, rawText));
		return $dillonkearns$elm_markdown$Markdown$InlineParser$Match(
			{end: end, matches: matches, start: start, text: text, textEnd: textEnd, textStart: textStart, type_: type_});
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$tokensToMatches = F4(
	function (tokens, matches, references, rawText) {
		return A5($dillonkearns$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM, tokens, _List_Nil, matches, references, rawText);
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$parse = F2(
	function (refs, rawText_) {
		var rawText = $elm$core$String$trim(rawText_);
		var tokens = $dillonkearns$elm_markdown$Markdown$InlineParser$tokenize(rawText);
		return $dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(
			A3(
				$dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatches,
				rawText,
				_List_Nil,
				$dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatches(
					A4($dillonkearns$elm_markdown$Markdown$InlineParser$tokensToMatches, tokens, _List_Nil, refs, rawText))));
	});
var $dillonkearns$elm_markdown$Markdown$Parser$thisIsDefinitelyNotAnHtmlTag = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			$elm$parser$Parser$Advanced$token(
			A2(
				$elm$parser$Parser$Advanced$Token,
				' ',
				$elm$parser$Parser$Expecting(' '))),
			$elm$parser$Parser$Advanced$token(
			A2(
				$elm$parser$Parser$Advanced$Token,
				'>',
				$elm$parser$Parser$Expecting('>'))),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$chompIf,
					$elm$core$Char$isAlpha,
					$elm$parser$Parser$Expecting('Alpha')),
				$elm$parser$Parser$Advanced$chompWhile(
					function (c) {
						return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
							c,
							_Utils_chr('-'));
					})),
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						$elm$parser$Parser$Advanced$token(
						A2(
							$elm$parser$Parser$Advanced$Token,
							':',
							$elm$parser$Parser$Expecting(':'))),
						$elm$parser$Parser$Advanced$token(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'@',
							$elm$parser$Parser$Expecting('@'))),
						$elm$parser$Parser$Advanced$token(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'\\',
							$elm$parser$Parser$Expecting('\\'))),
						$elm$parser$Parser$Advanced$token(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'+',
							$elm$parser$Parser$Expecting('+'))),
						$elm$parser$Parser$Advanced$token(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'.',
							$elm$parser$Parser$Expecting('.')))
					])))
		]));
var $dillonkearns$elm_markdown$Markdown$Parser$parseAsParagraphInsteadOfHtmlBlock = $elm$parser$Parser$Advanced$backtrackable(
	A2(
		$elm$parser$Parser$Advanced$mapChompedString,
		F2(
			function (rawLine, _v0) {
				return $dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
					$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(rawLine));
			}),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$token(
						A2(
							$elm$parser$Parser$Advanced$Token,
							'<',
							$elm$parser$Parser$Expecting('<'))),
					$dillonkearns$elm_markdown$Markdown$Parser$thisIsDefinitelyNotAnHtmlTag),
				$dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
			$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$CodeBlock = F2(
	function (language, body) {
		return {body: body, language: language};
	});
var $dillonkearns$elm_markdown$Markdown$CodeBlock$infoString = function (fenceCharacter) {
	var toInfoString = F2(
		function (str, _v2) {
			var _v1 = $elm$core$String$trim(str);
			if (_v1 === '') {
				return $elm$core$Maybe$Nothing;
			} else {
				var trimmed = _v1;
				return $elm$core$Maybe$Just(trimmed);
			}
		});
	var _v0 = fenceCharacter.kind;
	if (_v0.$ === 'Backtick') {
		return A2(
			$elm$parser$Parser$Advanced$mapChompedString,
			toInfoString,
			$elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return (!_Utils_eq(
						c,
						_Utils_chr('`'))) && (!$dillonkearns$elm_markdown$Whitespace$isLineEnd(c));
				}));
	} else {
		return A2(
			$elm$parser$Parser$Advanced$mapChompedString,
			toInfoString,
			$elm$parser$Parser$Advanced$chompWhile(
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $dillonkearns$elm_markdown$Whitespace$isLineEnd)));
	}
};
var $dillonkearns$elm_markdown$Markdown$CodeBlock$Backtick = {$: 'Backtick'};
var $dillonkearns$elm_markdown$Parser$Token$backtick = A2(
	$elm$parser$Parser$Advanced$Token,
	'`',
	$elm$parser$Parser$Expecting('a \'`\''));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$backtick = {
	_char: _Utils_chr('`'),
	kind: $dillonkearns$elm_markdown$Markdown$CodeBlock$Backtick,
	token: $dillonkearns$elm_markdown$Parser$Token$backtick
};
var $dillonkearns$elm_markdown$Markdown$CodeBlock$colToIndentation = function (_int) {
	switch (_int) {
		case 1:
			return $elm$parser$Parser$Advanced$succeed(0);
		case 2:
			return $elm$parser$Parser$Advanced$succeed(1);
		case 3:
			return $elm$parser$Parser$Advanced$succeed(2);
		case 4:
			return $elm$parser$Parser$Advanced$succeed(3);
		default:
			return $elm$parser$Parser$Advanced$problem(
				$elm$parser$Parser$Expecting('Fenced code blocks should be indented no more than 3 spaces'));
	}
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast = F2(
	function (minLength, fenceCharacter) {
		var builtTokens = A3(
			$elm$core$List$foldl,
			F2(
				function (t, p) {
					return A2($elm$parser$Parser$Advanced$ignorer, p, t);
				}),
			$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
			A2(
				$elm$core$List$repeat,
				minLength,
				$elm$parser$Parser$Advanced$token(fenceCharacter.token)));
		return A2(
			$elm$parser$Parser$Advanced$mapChompedString,
			F2(
				function (str, _v0) {
					return _Utils_Tuple2(
						fenceCharacter,
						$elm$core$String$length(str));
				}),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				builtTokens,
				$elm$parser$Parser$Advanced$chompWhile(
					$elm$core$Basics$eq(fenceCharacter._char))));
	});
var $elm$parser$Parser$Advanced$getCol = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.col, s);
	});
var $dillonkearns$elm_markdown$Markdown$CodeBlock$Tilde = {$: 'Tilde'};
var $dillonkearns$elm_markdown$Parser$Token$tilde = A2(
	$elm$parser$Parser$Advanced$Token,
	'~',
	$elm$parser$Parser$Expecting('a `~`'));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$tilde = {
	_char: _Utils_chr('~'),
	kind: $dillonkearns$elm_markdown$Markdown$CodeBlock$Tilde,
	token: $dillonkearns$elm_markdown$Parser$Token$tilde
};
var $dillonkearns$elm_markdown$Whitespace$space = $elm$parser$Parser$Advanced$token($dillonkearns$elm_markdown$Parser$Token$space);
var $dillonkearns$elm_markdown$Whitespace$upToThreeSpaces = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$dillonkearns$elm_markdown$Whitespace$space,
				$elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							$dillonkearns$elm_markdown$Whitespace$space,
							$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
						]))),
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						$dillonkearns$elm_markdown$Whitespace$space,
						$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
					]))),
			$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
		]));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$openingFence = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(
				F2(
					function (indent, _v0) {
						var character = _v0.a;
						var length = _v0.b;
						return {character: character, indented: indent, length: length};
					})),
			$dillonkearns$elm_markdown$Whitespace$upToThreeSpaces),
		A2($elm$parser$Parser$Advanced$andThen, $dillonkearns$elm_markdown$Markdown$CodeBlock$colToIndentation, $elm$parser$Parser$Advanced$getCol)),
	$elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2($dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast, 3, $dillonkearns$elm_markdown$Markdown$CodeBlock$backtick),
				A2($dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast, 3, $dillonkearns$elm_markdown$Markdown$CodeBlock$tilde)
			])));
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $dillonkearns$elm_markdown$Whitespace$isSpace = $elm$core$Basics$eq(
	_Utils_chr(' '));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$closingFence = F2(
	function (minLength, fenceCharacter) {
		return A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
						$dillonkearns$elm_markdown$Whitespace$upToThreeSpaces),
					A2($dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast, minLength, fenceCharacter)),
				$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpace)),
			$dillonkearns$elm_markdown$Helpers$lineEndOrEnd);
	});
var $dillonkearns$elm_markdown$Parser$Extra$upTo = F2(
	function (n, parser) {
		var _v0 = A2($elm$core$List$repeat, n, parser);
		if (!_v0.b) {
			return $elm$parser$Parser$Advanced$succeed(_Utils_Tuple0);
		} else {
			var firstParser = _v0.a;
			var remainingParsers = _v0.b;
			return A3(
				$elm$core$List$foldl,
				F2(
					function (p, parsers) {
						return $elm$parser$Parser$Advanced$oneOf(
							_List_fromArray(
								[
									A2($elm$parser$Parser$Advanced$ignorer, p, parsers),
									$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
								]));
					}),
				$elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							firstParser,
							$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
						])),
				remainingParsers);
		}
	});
var $dillonkearns$elm_markdown$Markdown$CodeBlock$codeBlockLine = function (indented) {
	return A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			A2($dillonkearns$elm_markdown$Parser$Extra$upTo, indented, $dillonkearns$elm_markdown$Whitespace$space)),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2($elm$parser$Parser$Advanced$ignorer, $elm$parser$Parser$Advanced$getOffset, $dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
			$dillonkearns$elm_markdown$Helpers$lineEndOrEnd));
};
var $elm$parser$Parser$Advanced$getSource = $elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, s.src, s);
	});
var $dillonkearns$elm_markdown$Markdown$CodeBlock$remainingBlockHelp = function (_v0) {
	var fence = _v0.a;
	var body = _v0.b;
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed(
					$elm$parser$Parser$Advanced$Done(body)),
				$elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd)),
				A2(
				$elm$parser$Parser$Advanced$mapChompedString,
				F2(
					function (lineEnd, _v1) {
						return $elm$parser$Parser$Advanced$Loop(
							_Utils_Tuple2(
								fence,
								_Utils_ap(body, lineEnd)));
					}),
				$dillonkearns$elm_markdown$Whitespace$lineEnd),
				$elm$parser$Parser$Advanced$backtrackable(
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed(
						$elm$parser$Parser$Advanced$Done(body)),
					A2($dillonkearns$elm_markdown$Markdown$CodeBlock$closingFence, fence.length, fence.character))),
				A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$keeper,
					A2(
						$elm$parser$Parser$Advanced$keeper,
						$elm$parser$Parser$Advanced$succeed(
							F3(
								function (start, end, source) {
									return $elm$parser$Parser$Advanced$Loop(
										_Utils_Tuple2(
											fence,
											_Utils_ap(
												body,
												A3($elm$core$String$slice, start, end, source))));
								})),
						$dillonkearns$elm_markdown$Markdown$CodeBlock$codeBlockLine(fence.indented)),
					$elm$parser$Parser$Advanced$getOffset),
				$elm$parser$Parser$Advanced$getSource)
			]));
};
var $dillonkearns$elm_markdown$Markdown$CodeBlock$remainingBlock = function (fence) {
	return A2(
		$elm$parser$Parser$Advanced$loop,
		_Utils_Tuple2(fence, ''),
		$dillonkearns$elm_markdown$Markdown$CodeBlock$remainingBlockHelp);
};
var $dillonkearns$elm_markdown$Markdown$CodeBlock$parser = A2(
	$elm$parser$Parser$Advanced$andThen,
	function (fence) {
		return A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$keeper,
				$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$CodeBlock$CodeBlock),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$dillonkearns$elm_markdown$Markdown$CodeBlock$infoString(fence.character),
					$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)),
			$dillonkearns$elm_markdown$Markdown$CodeBlock$remainingBlock(fence));
	},
	$dillonkearns$elm_markdown$Markdown$CodeBlock$openingFence);
var $dillonkearns$elm_markdown$Markdown$Heading$dropTrailingHashes = function (headingString) {
	return A2($elm$core$String$endsWith, '#', headingString) ? $dillonkearns$elm_markdown$Markdown$Heading$dropTrailingHashes(
		A2($elm$core$String$dropRight, 1, headingString)) : headingString;
};
var $elm$core$String$trimRight = _String_trimRight;
var $dillonkearns$elm_markdown$Markdown$Heading$dropClosingSequence = function (headingString) {
	var droppedTrailingHashesString = $dillonkearns$elm_markdown$Markdown$Heading$dropTrailingHashes(headingString);
	return (A2($elm$core$String$endsWith, ' ', droppedTrailingHashesString) || $elm$core$String$isEmpty(droppedTrailingHashesString)) ? $elm$core$String$trimRight(droppedTrailingHashesString) : headingString;
};
var $dillonkearns$elm_markdown$Parser$Token$hash = A2(
	$elm$parser$Parser$Advanced$Token,
	'#',
	$elm$parser$Parser$Expecting('a `#`'));
var $dillonkearns$elm_markdown$Markdown$Heading$isHash = function (c) {
	if ('#' === c.valueOf()) {
		return true;
	} else {
		return false;
	}
};
var $elm$parser$Parser$Advanced$spaces = $elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return _Utils_eq(
			c,
			_Utils_chr(' ')) || (_Utils_eq(
			c,
			_Utils_chr('\n')) || _Utils_eq(
			c,
			_Utils_chr('\r')));
	});
var $dillonkearns$elm_markdown$Markdown$Heading$parser = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$RawBlock$Heading),
				A2(
					$elm$parser$Parser$Advanced$andThen,
					function (startingSpaces) {
						var startSpace = $elm$core$String$length(startingSpaces);
						return (startSpace >= 4) ? $elm$parser$Parser$Advanced$problem(
							$elm$parser$Parser$Expecting('heading with < 4 spaces in front')) : $elm$parser$Parser$Advanced$succeed(startSpace);
					},
					$elm$parser$Parser$Advanced$getChompedString($elm$parser$Parser$Advanced$spaces))),
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$hash)),
		A2(
			$elm$parser$Parser$Advanced$andThen,
			function (additionalHashes) {
				var level = $elm$core$String$length(additionalHashes) + 1;
				return (level >= 7) ? $elm$parser$Parser$Advanced$problem(
					$elm$parser$Parser$Expecting('heading with < 7 #\'s')) : $elm$parser$Parser$Advanced$succeed(level);
			},
			$elm$parser$Parser$Advanced$getChompedString(
				$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Markdown$Heading$isHash)))),
	$elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed(
					$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines('')),
				$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$newline)),
				A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
					$elm$parser$Parser$Advanced$oneOf(
						_List_fromArray(
							[
								$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$space),
								$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$tab)
							]))),
				A2(
					$elm$parser$Parser$Advanced$mapChompedString,
					F2(
						function (headingText, _v0) {
							return $dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(
								$dillonkearns$elm_markdown$Markdown$Heading$dropClosingSequence(
									$elm$core$String$trim(headingText)));
						}),
					$dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd))
			])));
var $elm$parser$Parser$Advanced$findSubString = _Parser_findSubString;
var $elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var $elm$parser$Parser$Advanced$chompUntil = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$findSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A4($elm$parser$Parser$Advanced$fromInfo, newRow, newCol, expecting, s.context)) : A3(
				$elm$parser$Parser$Advanced$Good,
				_Utils_cmp(s.offset, newOffset) < 0,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $dillonkearns$elm_markdown$Parser$Token$greaterThan = A2(
	$elm$parser$Parser$Advanced$Token,
	'>',
	$elm$parser$Parser$Expecting('a `>`'));
var $elm$parser$Parser$Advanced$Located = F3(
	function (row, col, context) {
		return {col: col, context: context, row: row};
	});
var $elm$parser$Parser$Advanced$changeContext = F2(
	function (newContext, s) {
		return {col: s.col, context: newContext, indent: s.indent, offset: s.offset, row: s.row, src: s.src};
	});
var $elm$parser$Parser$Advanced$inContext = F2(
	function (context, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(
					A2(
						$elm$parser$Parser$Advanced$changeContext,
						A2(
							$elm$core$List$cons,
							A3($elm$parser$Parser$Advanced$Located, s0.row, s0.col, context),
							s0.context),
						s0));
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						a,
						A2($elm$parser$Parser$Advanced$changeContext, s0.context, s1));
				} else {
					var step = _v1;
					return step;
				}
			});
	});
var $dillonkearns$elm_markdown$Whitespace$isWhitespace = function (_char) {
	switch (_char.valueOf()) {
		case ' ':
			return true;
		case '\n':
			return true;
		case '\t':
			return true;
		case '\u000B':
			return true;
		case '\u000C':
			return true;
		case '\r':
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$Parser$Token$lessThan = A2(
	$elm$parser$Parser$Advanced$Token,
	'<',
	$elm$parser$Parser$Expecting('a `<`'));
var $dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$destinationParser = A2(
	$elm$parser$Parser$Advanced$inContext,
	'link destination',
	$elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed($elm$url$Url$percentEncode),
					$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$lessThan)),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$getChompedString(
						$elm$parser$Parser$Advanced$chompUntil($dillonkearns$elm_markdown$Parser$Token$greaterThan)),
					$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$greaterThan))),
				$elm$parser$Parser$Advanced$getChompedString(
				$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore(
					A2($elm$core$Basics$composeL, $elm$core$Basics$not, $dillonkearns$elm_markdown$Whitespace$isWhitespace)))
			])));
var $dillonkearns$elm_markdown$Parser$Token$closingSquareBracket = A2(
	$elm$parser$Parser$Advanced$Token,
	']',
	$elm$parser$Parser$Expecting('a `]`'));
var $dillonkearns$elm_markdown$Parser$Token$openingSquareBracket = A2(
	$elm$parser$Parser$Advanced$Token,
	'[',
	$elm$parser$Parser$Expecting('a `[`'));
var $dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$labelParser = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$Helpers$prepareRefLabel),
		$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$openingSquareBracket)),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString(
			$elm$parser$Parser$Advanced$chompUntil($dillonkearns$elm_markdown$Parser$Token$closingSquareBracket)),
		$elm$parser$Parser$Advanced$symbol(
			A2(
				$elm$parser$Parser$Advanced$Token,
				']:',
				$elm$parser$Parser$Expecting(']:')))));
var $dillonkearns$elm_markdown$Parser$Token$doubleQuote = A2(
	$elm$parser$Parser$Advanced$Token,
	'\"',
	$elm$parser$Parser$Expecting('a double quote'));
var $dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$hasNoBlankLine = function (str) {
	return A2($elm$core$String$contains, '\n\n', str) ? $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Expecting('no blank line')) : $elm$parser$Parser$Advanced$succeed(str);
};
var $dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$onlyWhitespaceTillNewline = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$chompWhile(
		function (c) {
			return (!$dillonkearns$elm_markdown$Whitespace$isLineEnd(c)) && $dillonkearns$elm_markdown$Whitespace$isWhitespace(c);
		}),
	$dillonkearns$elm_markdown$Helpers$lineEndOrEnd);
var $dillonkearns$elm_markdown$Whitespace$requiredWhitespace = A2(
	$elm$parser$Parser$Advanced$ignorer,
	A2(
		$elm$parser$Parser$Advanced$chompIf,
		$dillonkearns$elm_markdown$Whitespace$isWhitespace,
		$elm$parser$Parser$Expecting('Required whitespace')),
	$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isWhitespace));
var $dillonkearns$elm_markdown$Parser$Token$singleQuote = A2(
	$elm$parser$Parser$Advanced$Token,
	'\'',
	$elm$parser$Parser$Expecting('a single quote'));
var $dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$titleParser = function () {
	var inSingleQuotes = A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Maybe$Just),
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$singleQuote)),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$andThen,
					$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$hasNoBlankLine,
					$elm$parser$Parser$Advanced$getChompedString(
						$elm$parser$Parser$Advanced$chompUntil($dillonkearns$elm_markdown$Parser$Token$singleQuote))),
				$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$singleQuote)),
			$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$onlyWhitespaceTillNewline));
	var inDoubleQuotes = A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Maybe$Just),
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$doubleQuote)),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$andThen,
					$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$hasNoBlankLine,
					$elm$parser$Parser$Advanced$getChompedString(
						$elm$parser$Parser$Advanced$chompUntil($dillonkearns$elm_markdown$Parser$Token$doubleQuote))),
				$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$doubleQuote)),
			$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$onlyWhitespaceTillNewline));
	return A2(
		$elm$parser$Parser$Advanced$inContext,
		'title',
		$elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$Advanced$backtrackable(
					A2(
						$elm$parser$Parser$Advanced$keeper,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
							$dillonkearns$elm_markdown$Whitespace$requiredWhitespace),
						$elm$parser$Parser$Advanced$oneOf(
							_List_fromArray(
								[
									inDoubleQuotes,
									inSingleQuotes,
									$elm$parser$Parser$Advanced$succeed($elm$core$Maybe$Nothing)
								])))),
					A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed($elm$core$Maybe$Nothing),
					$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$onlyWhitespaceTillNewline)
				])));
}();
var $dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$parser = A2(
	$elm$parser$Parser$Advanced$inContext,
	'link reference definition',
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed(
						F3(
							function (label, destination, title) {
								return _Utils_Tuple2(
									label,
									{destination: destination, title: title});
							})),
					$dillonkearns$elm_markdown$Whitespace$upToThreeSpaces),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$labelParser,
							$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab)),
						$elm$parser$Parser$Advanced$oneOf(
							_List_fromArray(
								[
									$dillonkearns$elm_markdown$Whitespace$lineEnd,
									$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
								]))),
					$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab))),
			$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$destinationParser),
		$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$titleParser));
var $dillonkearns$elm_markdown$ThematicBreak$ThematicBreak = {$: 'ThematicBreak'};
var $dillonkearns$elm_markdown$ThematicBreak$whitespace = $elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab);
var $dillonkearns$elm_markdown$ThematicBreak$withChar = function (tchar) {
	var token = $dillonkearns$elm_markdown$Parser$Token$parseString(
		$elm$core$String$fromChar(tchar));
	return A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2(
								$elm$parser$Parser$Advanced$ignorer,
								$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$ThematicBreak$ThematicBreak),
								token),
							$dillonkearns$elm_markdown$ThematicBreak$whitespace),
						token),
					$dillonkearns$elm_markdown$ThematicBreak$whitespace),
				token),
			$elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return _Utils_eq(c, tchar) || $dillonkearns$elm_markdown$Whitespace$isSpaceOrTab(c);
				})),
		$dillonkearns$elm_markdown$Helpers$lineEndOrEnd);
};
var $dillonkearns$elm_markdown$ThematicBreak$parseThematicBreak = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			$dillonkearns$elm_markdown$ThematicBreak$withChar(
			_Utils_chr('-')),
			$dillonkearns$elm_markdown$ThematicBreak$withChar(
			_Utils_chr('*')),
			$dillonkearns$elm_markdown$ThematicBreak$withChar(
			_Utils_chr('_'))
		]));
var $dillonkearns$elm_markdown$ThematicBreak$parser = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
						$dillonkearns$elm_markdown$Whitespace$space),
					$elm$parser$Parser$Advanced$oneOf(
						_List_fromArray(
							[
								$dillonkearns$elm_markdown$Whitespace$space,
								$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
							]))),
				$elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							$dillonkearns$elm_markdown$Whitespace$space,
							$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
						]))),
			$dillonkearns$elm_markdown$ThematicBreak$parseThematicBreak),
			$dillonkearns$elm_markdown$ThematicBreak$parseThematicBreak
		]));
var $dillonkearns$elm_markdown$Markdown$RawBlock$LevelOne = {$: 'LevelOne'};
var $dillonkearns$elm_markdown$Markdown$RawBlock$LevelTwo = {$: 'LevelTwo'};
var $dillonkearns$elm_markdown$Markdown$RawBlock$SetextLine = F2(
	function (a, b) {
		return {$: 'SetextLine', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Parser$Token$equals = A2(
	$elm$parser$Parser$Advanced$Token,
	'=',
	$elm$parser$Parser$Expecting('a `=`'));
var $dillonkearns$elm_markdown$Parser$Token$minus = A2(
	$elm$parser$Parser$Advanced$Token,
	'-',
	$elm$parser$Parser$Expecting('a `-`'));
var $dillonkearns$elm_markdown$Markdown$Parser$setextLineParser = function () {
	var setextLevel = F3(
		function (level, levelToken, levelChar) {
			return A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed(level),
					$elm$parser$Parser$Advanced$token(levelToken)),
				$elm$parser$Parser$Advanced$chompWhile(
					$elm$core$Basics$eq(levelChar)));
		});
	return A2(
		$elm$parser$Parser$Advanced$mapChompedString,
		F2(
			function (raw, level) {
				return A2($dillonkearns$elm_markdown$Markdown$RawBlock$SetextLine, level, raw);
			}),
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$dillonkearns$elm_markdown$Whitespace$upToThreeSpaces),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$oneOf(
						_List_fromArray(
							[
								A3(
								setextLevel,
								$dillonkearns$elm_markdown$Markdown$RawBlock$LevelOne,
								$dillonkearns$elm_markdown$Parser$Token$equals,
								_Utils_chr('=')),
								A3(
								setextLevel,
								$dillonkearns$elm_markdown$Markdown$RawBlock$LevelTwo,
								$dillonkearns$elm_markdown$Parser$Token$minus,
								_Utils_chr('-'))
							])),
					$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab)),
				$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)));
}();
var $dillonkearns$elm_markdown$Markdown$RawBlock$TableDelimiter = function (a) {
	return {$: 'TableDelimiter', a: a};
};
var $dillonkearns$elm_markdown$Markdown$TableParser$chompSinglelineWhitespace = $elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab);
var $dillonkearns$elm_markdown$Parser$Extra$maybeChomp = function (condition) {
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$chompIf,
				condition,
				$elm$parser$Parser$Problem('Character not found')),
				$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
			]));
};
var $dillonkearns$elm_markdown$Markdown$TableParser$requirePipeIfNotFirst = function (columns) {
	return $elm$core$List$isEmpty(columns) ? $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				$dillonkearns$elm_markdown$Parser$Token$parseString('|'),
				$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0)
			])) : $dillonkearns$elm_markdown$Parser$Token$parseString('|');
};
var $dillonkearns$elm_markdown$Markdown$TableParser$delimiterRowHelp = function (revDelimiterColumns) {
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				$elm$parser$Parser$Advanced$backtrackable(
				A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return $elm$parser$Parser$Advanced$Done(revDelimiterColumns);
					},
					$dillonkearns$elm_markdown$Parser$Token$parseString('|\n'))),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v1) {
					return $elm$parser$Parser$Advanced$Done(revDelimiterColumns);
				},
				$dillonkearns$elm_markdown$Parser$Token$parseString('\n')),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v2) {
					return $elm$parser$Parser$Advanced$Done(revDelimiterColumns);
				},
				$elm$parser$Parser$Advanced$end(
					$elm$parser$Parser$Expecting('end'))),
				$elm$parser$Parser$Advanced$backtrackable(
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						$elm$parser$Parser$Advanced$succeed(
							$elm$parser$Parser$Advanced$Done(revDelimiterColumns)),
						$dillonkearns$elm_markdown$Parser$Token$parseString('|')),
					$elm$parser$Parser$Advanced$end(
						$elm$parser$Parser$Expecting('end')))),
				A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						$elm$parser$Parser$Advanced$succeed(
							function (column) {
								return $elm$parser$Parser$Advanced$Loop(
									A2($elm$core$List$cons, column, revDelimiterColumns));
							}),
						$dillonkearns$elm_markdown$Markdown$TableParser$requirePipeIfNotFirst(revDelimiterColumns)),
					$dillonkearns$elm_markdown$Markdown$TableParser$chompSinglelineWhitespace),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$getChompedString(
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2(
								$elm$parser$Parser$Advanced$ignorer,
								A2(
									$elm$parser$Parser$Advanced$ignorer,
									$elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
									$dillonkearns$elm_markdown$Parser$Extra$maybeChomp(
										function (c) {
											return _Utils_eq(
												c,
												_Utils_chr(':'));
										})),
								$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore(
									function (c) {
										return _Utils_eq(
											c,
											_Utils_chr('-'));
									})),
							$dillonkearns$elm_markdown$Parser$Extra$maybeChomp(
								function (c) {
									return _Utils_eq(
										c,
										_Utils_chr(':'));
								}))),
					$dillonkearns$elm_markdown$Markdown$TableParser$chompSinglelineWhitespace))
			]));
};
var $dillonkearns$elm_markdown$Markdown$Block$AlignCenter = {$: 'AlignCenter'};
var $dillonkearns$elm_markdown$Markdown$Block$AlignLeft = {$: 'AlignLeft'};
var $dillonkearns$elm_markdown$Markdown$Block$AlignRight = {$: 'AlignRight'};
var $dillonkearns$elm_markdown$Markdown$TableParser$delimiterToAlignment = function (cell) {
	var _v0 = _Utils_Tuple2(
		A2($elm$core$String$startsWith, ':', cell),
		A2($elm$core$String$endsWith, ':', cell));
	if (_v0.a) {
		if (_v0.b) {
			return $elm$core$Maybe$Just($dillonkearns$elm_markdown$Markdown$Block$AlignCenter);
		} else {
			return $elm$core$Maybe$Just($dillonkearns$elm_markdown$Markdown$Block$AlignLeft);
		}
	} else {
		if (_v0.b) {
			return $elm$core$Maybe$Just($dillonkearns$elm_markdown$Markdown$Block$AlignRight);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}
};
var $dillonkearns$elm_markdown$Markdown$TableParser$delimiterRowParser = A2(
	$elm$parser$Parser$Advanced$andThen,
	function (delimiterRow) {
		var trimmed = delimiterRow.a.trimmed;
		var headers = delimiterRow.b;
		return $elm$core$List$isEmpty(headers) ? $elm$parser$Parser$Advanced$problem(
			$elm$parser$Parser$Expecting('Must have at least one column in delimiter row.')) : ((($elm$core$List$length(headers) === 1) && (!(A2($elm$core$String$startsWith, '|', trimmed) && A2($elm$core$String$endsWith, '|', trimmed)))) ? $elm$parser$Parser$Advanced$problem(
			$elm$parser$Parser$Problem('Tables with a single column must have pipes at the start and end of the delimiter row to avoid ambiguity.')) : $elm$parser$Parser$Advanced$succeed(delimiterRow));
	},
	A2(
		$elm$parser$Parser$Advanced$mapChompedString,
		F2(
			function (delimiterText, revDelimiterColumns) {
				return A2(
					$dillonkearns$elm_markdown$Markdown$Table$TableDelimiterRow,
					{
						raw: delimiterText,
						trimmed: $elm$core$String$trim(delimiterText)
					},
					A2(
						$elm$core$List$map,
						$dillonkearns$elm_markdown$Markdown$TableParser$delimiterToAlignment,
						$elm$core$List$reverse(revDelimiterColumns)));
			}),
		A2($elm$parser$Parser$Advanced$loop, _List_Nil, $dillonkearns$elm_markdown$Markdown$TableParser$delimiterRowHelp)));
var $dillonkearns$elm_markdown$Markdown$Parser$tableDelimiterInOpenParagraph = A2($elm$parser$Parser$Advanced$map, $dillonkearns$elm_markdown$Markdown$RawBlock$TableDelimiter, $dillonkearns$elm_markdown$Markdown$TableParser$delimiterRowParser);
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $dillonkearns$elm_markdown$Markdown$TableParser$standardizeRowLength = F2(
	function (expectedLength, row) {
		var rowLength = $elm$core$List$length(row);
		var _v0 = A2($elm$core$Basics$compare, expectedLength, rowLength);
		switch (_v0.$) {
			case 'LT':
				return A2($elm$core$List$take, expectedLength, row);
			case 'EQ':
				return row;
			default:
				return _Utils_ap(
					row,
					A2($elm$core$List$repeat, expectedLength - rowLength, ''));
		}
	});
var $dillonkearns$elm_markdown$Markdown$TableParser$bodyRowParser = function (expectedRowLength) {
	return A2(
		$elm$parser$Parser$Advanced$andThen,
		function (row) {
			return ($elm$core$List$isEmpty(row) || A2($elm$core$List$all, $elm$core$String$isEmpty, row)) ? $elm$parser$Parser$Advanced$problem(
				$elm$parser$Parser$Problem('A line must have at least one column')) : $elm$parser$Parser$Advanced$succeed(
				A2($dillonkearns$elm_markdown$Markdown$TableParser$standardizeRowLength, expectedRowLength, row));
		},
		$dillonkearns$elm_markdown$Markdown$TableParser$rowParser);
};
var $dillonkearns$elm_markdown$Markdown$Parser$tableRowIfTableStarted = function (_v0) {
	var headers = _v0.a;
	var body = _v0.b;
	return A2(
		$elm$parser$Parser$Advanced$map,
		function (row) {
			return $dillonkearns$elm_markdown$Markdown$RawBlock$Table(
				A2(
					$dillonkearns$elm_markdown$Markdown$Table$Table,
					headers,
					_Utils_ap(
						body,
						_List_fromArray(
							[row]))));
		},
		$dillonkearns$elm_markdown$Markdown$TableParser$bodyRowParser(
			$elm$core$List$length(headers)));
};
var $dillonkearns$elm_markdown$Markdown$Block$H1 = {$: 'H1'};
var $dillonkearns$elm_markdown$Markdown$Block$H2 = {$: 'H2'};
var $dillonkearns$elm_markdown$Markdown$Block$H3 = {$: 'H3'};
var $dillonkearns$elm_markdown$Markdown$Block$H4 = {$: 'H4'};
var $dillonkearns$elm_markdown$Markdown$Block$H5 = {$: 'H5'};
var $dillonkearns$elm_markdown$Markdown$Block$H6 = {$: 'H6'};
var $dillonkearns$elm_markdown$Markdown$Parser$toHeading = function (level) {
	switch (level) {
		case 1:
			return $elm$core$Result$Ok($dillonkearns$elm_markdown$Markdown$Block$H1);
		case 2:
			return $elm$core$Result$Ok($dillonkearns$elm_markdown$Markdown$Block$H2);
		case 3:
			return $elm$core$Result$Ok($dillonkearns$elm_markdown$Markdown$Block$H3);
		case 4:
			return $elm$core$Result$Ok($dillonkearns$elm_markdown$Markdown$Block$H4);
		case 5:
			return $elm$core$Result$Ok($dillonkearns$elm_markdown$Markdown$Block$H5);
		case 6:
			return $elm$core$Result$Ok($dillonkearns$elm_markdown$Markdown$Block$H6);
		default:
			return $elm$core$Result$Err(
				$elm$parser$Parser$Expecting(
					'A heading with 1 to 6 #\'s, but found ' + $elm$core$String$fromInt(level)));
	}
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$UnorderedListBlock = function (a) {
	return {$: 'UnorderedListBlock', a: a};
};
var $dillonkearns$elm_markdown$Parser$Token$asterisk = A2(
	$elm$parser$Parser$Advanced$Token,
	'*',
	$elm$parser$Parser$Expecting('a `*`'));
var $dillonkearns$elm_markdown$Parser$Token$plus = A2(
	$elm$parser$Parser$Advanced$Token,
	'+',
	$elm$parser$Parser$Expecting('a `+`'));
var $dillonkearns$elm_markdown$Markdown$UnorderedList$listMarkerParser = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Parser$Token$minus),
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$minus)),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Parser$Token$plus),
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$plus)),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Parser$Token$asterisk),
			$elm$parser$Parser$Advanced$symbol($dillonkearns$elm_markdown$Parser$Token$asterisk))
		]));
var $dillonkearns$elm_markdown$Markdown$ListItem$PlainItem = function (a) {
	return {$: 'PlainItem', a: a};
};
var $dillonkearns$elm_markdown$Markdown$ListItem$TaskItem = F2(
	function (a, b) {
		return {$: 'TaskItem', a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$ListItem$Complete = {$: 'Complete'};
var $dillonkearns$elm_markdown$Markdown$ListItem$Incomplete = {$: 'Incomplete'};
var $dillonkearns$elm_markdown$Markdown$ListItem$taskItemParser = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$ListItem$Complete),
			$elm$parser$Parser$Advanced$symbol(
				A2(
					$elm$parser$Parser$Advanced$Token,
					'[x] ',
					$elm$parser$Parser$ExpectingSymbol('[x] ')))),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$ListItem$Complete),
			$elm$parser$Parser$Advanced$symbol(
				A2(
					$elm$parser$Parser$Advanced$Token,
					'[X] ',
					$elm$parser$Parser$ExpectingSymbol('[X] ')))),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$ListItem$Incomplete),
			$elm$parser$Parser$Advanced$symbol(
				A2(
					$elm$parser$Parser$Advanced$Token,
					'[ ] ',
					$elm$parser$Parser$ExpectingSymbol('[ ] '))))
		]));
var $dillonkearns$elm_markdown$Markdown$ListItem$parser = A2(
	$elm$parser$Parser$Advanced$keeper,
	$elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$keeper,
				$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$ListItem$TaskItem),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$dillonkearns$elm_markdown$Markdown$ListItem$taskItemParser,
					$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab))),
				$elm$parser$Parser$Advanced$succeed($dillonkearns$elm_markdown$Markdown$ListItem$PlainItem)
			])),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString($dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
		$dillonkearns$elm_markdown$Helpers$lineEndOrEnd));
var $dillonkearns$elm_markdown$Markdown$UnorderedList$itemBody = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$Advanced$backtrackable(
					$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab))),
			$dillonkearns$elm_markdown$Markdown$ListItem$parser),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$ListItem$PlainItem('')),
			$dillonkearns$elm_markdown$Whitespace$lineEnd)
		]));
var $dillonkearns$elm_markdown$Markdown$UnorderedList$singleItemParser = function (listMarker) {
	return A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$elm$parser$Parser$Advanced$backtrackable(
				$elm$parser$Parser$Advanced$symbol(listMarker))),
		$dillonkearns$elm_markdown$Markdown$UnorderedList$itemBody);
};
var $dillonkearns$elm_markdown$Markdown$UnorderedList$statementsHelp = F3(
	function (itemParser, firstItem, revStmts) {
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$Advanced$map,
					function (stmt) {
						return $elm$parser$Parser$Advanced$Loop(
							A2($elm$core$List$cons, stmt, revStmts));
					},
					itemParser),
					$elm$parser$Parser$Advanced$succeed(
					$elm$parser$Parser$Advanced$Done(
						A2(
							$elm$core$List$cons,
							firstItem,
							$elm$core$List$reverse(revStmts))))
				]));
	});
var $dillonkearns$elm_markdown$Markdown$UnorderedList$parser = function () {
	var parseSubsequentItems = F2(
		function (listMarker, firstItem) {
			return A2(
				$elm$parser$Parser$Advanced$loop,
				_List_Nil,
				A2(
					$dillonkearns$elm_markdown$Markdown$UnorderedList$statementsHelp,
					$dillonkearns$elm_markdown$Markdown$UnorderedList$singleItemParser(listMarker),
					firstItem));
		});
	return A2(
		$elm$parser$Parser$Advanced$andThen,
		$elm$core$Basics$identity,
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$keeper,
				$elm$parser$Parser$Advanced$succeed(parseSubsequentItems),
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$UnorderedList$listMarkerParser),
					$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab))),
			$dillonkearns$elm_markdown$Markdown$ListItem$parser));
}();
var $dillonkearns$elm_markdown$Markdown$Parser$unorderedListBlock = function () {
	var parseListItem = function (unparsedListItem) {
		if (unparsedListItem.$ === 'TaskItem') {
			var completion = unparsedListItem.a;
			var body = unparsedListItem.b;
			return {
				body: $dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(body),
				task: $elm$core$Maybe$Just(
					function () {
						if (completion.$ === 'Complete') {
							return true;
						} else {
							return false;
						}
					}())
			};
		} else {
			var body = unparsedListItem.a;
			return {
				body: $dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(body),
				task: $elm$core$Maybe$Nothing
			};
		}
	};
	return A2(
		$elm$parser$Parser$Advanced$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$map(parseListItem),
			$dillonkearns$elm_markdown$Markdown$RawBlock$UnorderedListBlock),
		$dillonkearns$elm_markdown$Markdown$UnorderedList$parser);
}();
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$childToBlocks = F2(
	function (node, blocks) {
		switch (node.$) {
			case 'Element':
				var tag = node.a;
				var attributes = node.b;
				var children = node.c;
				var _v28 = $dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocks(children);
				if (_v28.$ === 'Ok') {
					var childrenAsBlocks = _v28.a;
					var block = $dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
						A3($dillonkearns$elm_markdown$Markdown$Block$HtmlElement, tag, attributes, childrenAsBlocks));
					return $elm$core$Result$Ok(
						A2($elm$core$List$cons, block, blocks));
				} else {
					var err = _v28.a;
					return $elm$core$Result$Err(err);
				}
			case 'Text':
				var innerText = node.a;
				var _v29 = $dillonkearns$elm_markdown$Markdown$Parser$parse(innerText);
				if (_v29.$ === 'Ok') {
					var value = _v29.a;
					return $elm$core$Result$Ok(
						_Utils_ap(
							$elm$core$List$reverse(value),
							blocks));
				} else {
					var error = _v29.a;
					return $elm$core$Result$Err(
						$elm$parser$Parser$Expecting(
							A2(
								$elm$core$String$join,
								'\n',
								A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$deadEndToString, error))));
				}
			case 'Comment':
				var string = node.a;
				return $elm$core$Result$Ok(
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							$dillonkearns$elm_markdown$Markdown$Block$HtmlComment(string)),
						blocks));
			case 'Cdata':
				var string = node.a;
				return $elm$core$Result$Ok(
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							$dillonkearns$elm_markdown$Markdown$Block$Cdata(string)),
						blocks));
			case 'ProcessingInstruction':
				var string = node.a;
				return $elm$core$Result$Ok(
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							$dillonkearns$elm_markdown$Markdown$Block$ProcessingInstruction(string)),
						blocks));
			default:
				var declarationType = node.a;
				var content = node.b;
				return $elm$core$Result$Ok(
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							A2($dillonkearns$elm_markdown$Markdown$Block$HtmlDeclaration, declarationType, content)),
						blocks));
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper = F2(
	function (referencesDict, _v23) {
		var unparsedInlines = _v23.a;
		var mappedReferencesDict = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$mapSecond(
					function (_v24) {
						var destination = _v24.destination;
						var title = _v24.title;
						return _Utils_Tuple2(destination, title);
					}),
				referencesDict));
		return A2(
			$elm$core$List$map,
			$dillonkearns$elm_markdown$Markdown$Parser$mapInline,
			A2($dillonkearns$elm_markdown$Markdown$InlineParser$parse, mappedReferencesDict, unparsedInlines));
	});
var $dillonkearns$elm_markdown$Markdown$Parser$mapInline = function (inline) {
	switch (inline.$) {
		case 'Text':
			var string = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$Text(string);
		case 'HardLineBreak':
			return $dillonkearns$elm_markdown$Markdown$Block$HardLineBreak;
		case 'CodeInline':
			var string = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$CodeSpan(string);
		case 'Link':
			var string = inline.a;
			var maybeString = inline.b;
			var inlines = inline.c;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Block$Link,
				string,
				maybeString,
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
		case 'Image':
			var string = inline.a;
			var maybeString = inline.b;
			var inlines = inline.c;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Block$Image,
				string,
				maybeString,
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
		case 'HtmlInline':
			var node = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$HtmlInline(
				$dillonkearns$elm_markdown$Markdown$Parser$nodeToRawBlock(node));
		case 'Emphasis':
			var level = inline.a;
			var inlines = inline.b;
			switch (level) {
				case 1:
					return $dillonkearns$elm_markdown$Markdown$Block$Emphasis(
						A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
				case 2:
					return $dillonkearns$elm_markdown$Markdown$Block$Strong(
						A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
				default:
					return $dillonkearns$elm_markdown$Markdown$Block$Strong(
						A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
			}
		default:
			var inlines = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$Strikethrough(
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
	}
};
var $dillonkearns$elm_markdown$Markdown$Parser$nodeToRawBlock = function (node) {
	switch (node.$) {
		case 'Text':
			var innerText = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$HtmlComment('TODO this never happens, but use types to drop this case.');
		case 'Element':
			var tag = node.a;
			var attributes = node.b;
			var children = node.c;
			var parseChild = function (child) {
				if (child.$ === 'Text') {
					var text = child.a;
					return $dillonkearns$elm_markdown$Markdown$Parser$textNodeToBlocks(text);
				} else {
					return _List_fromArray(
						[
							$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							$dillonkearns$elm_markdown$Markdown$Parser$nodeToRawBlock(child))
						]);
				}
			};
			return A3(
				$dillonkearns$elm_markdown$Markdown$Block$HtmlElement,
				tag,
				attributes,
				A2($elm$core$List$concatMap, parseChild, children));
		case 'Comment':
			var string = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$HtmlComment(string);
		case 'Cdata':
			var string = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$Cdata(string);
		case 'ProcessingInstruction':
			var string = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$ProcessingInstruction(string);
		default:
			var declarationType = node.a;
			var content = node.b;
			return A2($dillonkearns$elm_markdown$Markdown$Block$HtmlDeclaration, declarationType, content);
	}
};
var $dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocks = function (children) {
	return A2($dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocksHelp, children, _List_Nil);
};
var $dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocksHelp = F2(
	function (remaining, soFar) {
		nodesToBlocksHelp:
		while (true) {
			if (remaining.b) {
				var node = remaining.a;
				var rest = remaining.b;
				var _v18 = A2($dillonkearns$elm_markdown$Markdown$Parser$childToBlocks, node, soFar);
				if (_v18.$ === 'Ok') {
					var newSoFar = _v18.a;
					var $temp$remaining = rest,
						$temp$soFar = newSoFar;
					remaining = $temp$remaining;
					soFar = $temp$soFar;
					continue nodesToBlocksHelp;
				} else {
					var e = _v18.a;
					return $elm$core$Result$Err(e);
				}
			} else {
				return $elm$core$Result$Ok(
					$elm$core$List$reverse(soFar));
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$parse = function (input) {
	var _v14 = A2(
		$elm$parser$Parser$Advanced$run,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$dillonkearns$elm_markdown$Markdown$Parser$cyclic$rawBlockParser(),
			$dillonkearns$elm_markdown$Helpers$endOfFile),
		input);
	if (_v14.$ === 'Err') {
		var e = _v14.a;
		return $elm$core$Result$Err(e);
	} else {
		var v = _v14.a;
		var _v15 = $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlines(v);
		if (_v15.$ === 'Err') {
			var e = _v15.a;
			return A2(
				$elm$parser$Parser$Advanced$run,
				$elm$parser$Parser$Advanced$problem(e),
				'');
		} else {
			var blocks = _v15.a;
			var isNotEmptyParagraph = function (block) {
				if ((block.$ === 'Paragraph') && (!block.a.b)) {
					return false;
				} else {
					return true;
				}
			};
			return $elm$core$Result$Ok(
				A2($elm$core$List$filter, isNotEmptyParagraph, blocks));
		}
	}
};
var $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlines = function (state) {
	return A3($dillonkearns$elm_markdown$Markdown$Parser$parseAllInlinesHelp, state, state.rawBlocks, _List_Nil);
};
var $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlinesHelp = F3(
	function (state, rawBlocks, parsedBlocks) {
		parseAllInlinesHelp:
		while (true) {
			if (rawBlocks.b) {
				var rawBlock = rawBlocks.a;
				var rest = rawBlocks.b;
				var _v13 = A2($dillonkearns$elm_markdown$Markdown$Parser$parseInlines, state.linkReferenceDefinitions, rawBlock);
				switch (_v13.$) {
					case 'ParsedBlock':
						var newParsedBlock = _v13.a;
						var $temp$state = state,
							$temp$rawBlocks = rest,
							$temp$parsedBlocks = A2($elm$core$List$cons, newParsedBlock, parsedBlocks);
						state = $temp$state;
						rawBlocks = $temp$rawBlocks;
						parsedBlocks = $temp$parsedBlocks;
						continue parseAllInlinesHelp;
					case 'EmptyBlock':
						var $temp$state = state,
							$temp$rawBlocks = rest,
							$temp$parsedBlocks = parsedBlocks;
						state = $temp$state;
						rawBlocks = $temp$rawBlocks;
						parsedBlocks = $temp$parsedBlocks;
						continue parseAllInlinesHelp;
					default:
						var e = _v13.a;
						return $elm$core$Result$Err(e);
				}
			} else {
				return $elm$core$Result$Ok(parsedBlocks);
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$parseHeaderInlines = F2(
	function (linkReferences, header) {
		return A2(
			$elm$core$List$map,
			function (_v11) {
				var label = _v11.label;
				var alignment = _v11.alignment;
				return A3(
					$dillonkearns$elm_markdown$Markdown$Parser$parseRawInline,
					linkReferences,
					function (parsedHeaderLabel) {
						return {alignment: alignment, label: parsedHeaderLabel};
					},
					$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(label));
			},
			header);
	});
var $dillonkearns$elm_markdown$Markdown$Parser$parseInlines = F2(
	function (linkReferences, rawBlock) {
		switch (rawBlock.$) {
			case 'Heading':
				var level = rawBlock.a;
				var unparsedInlines = rawBlock.b;
				var _v5 = $dillonkearns$elm_markdown$Markdown$Parser$toHeading(level);
				if (_v5.$ === 'Ok') {
					var parsedLevel = _v5.a;
					return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
						A2(
							$dillonkearns$elm_markdown$Markdown$Block$Heading,
							parsedLevel,
							A2($dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper, linkReferences, unparsedInlines)));
				} else {
					var e = _v5.a;
					return $dillonkearns$elm_markdown$Markdown$Parser$InlineProblem(e);
				}
			case 'OpenBlockOrParagraph':
				var unparsedInlines = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$Paragraph(
						A2($dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper, linkReferences, unparsedInlines)));
			case 'Html':
				var html = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(html));
			case 'UnorderedListBlock':
				var unparsedItems = rawBlock.a;
				var parseItem = function (unparsed) {
					var task = function () {
						var _v6 = unparsed.task;
						if (_v6.$ === 'Just') {
							if (!_v6.a) {
								return $dillonkearns$elm_markdown$Markdown$Block$IncompleteTask;
							} else {
								return $dillonkearns$elm_markdown$Markdown$Block$CompletedTask;
							}
						} else {
							return $dillonkearns$elm_markdown$Markdown$Block$NoTask;
						}
					}();
					var parsedInlines = A3($dillonkearns$elm_markdown$Markdown$Parser$parseRawInline, linkReferences, $elm$core$Basics$identity, unparsed.body);
					return A2($dillonkearns$elm_markdown$Markdown$Block$ListItem, task, parsedInlines);
				};
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$UnorderedList(
						A2($elm$core$List$map, parseItem, unparsedItems)));
			case 'OrderedListBlock':
				var startingIndex = rawBlock.a;
				var unparsedInlines = rawBlock.b;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					A2(
						$dillonkearns$elm_markdown$Markdown$Block$OrderedList,
						startingIndex,
						A2(
							$elm$core$List$map,
							A2($dillonkearns$elm_markdown$Markdown$Parser$parseRawInline, linkReferences, $elm$core$Basics$identity),
							unparsedInlines)));
			case 'CodeBlock':
				var codeBlock = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$CodeBlock(codeBlock));
			case 'ThematicBreak':
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock($dillonkearns$elm_markdown$Markdown$Block$ThematicBreak);
			case 'BlankLine':
				return $dillonkearns$elm_markdown$Markdown$Parser$EmptyBlock;
			case 'BlockQuote':
				var rawBlocks = rawBlock.a;
				var _v7 = A2(
					$elm$parser$Parser$Advanced$run,
					$dillonkearns$elm_markdown$Markdown$Parser$cyclic$rawBlockParser(),
					rawBlocks);
				if (_v7.$ === 'Ok') {
					var value = _v7.a;
					var _v8 = $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlines(value);
					if (_v8.$ === 'Ok') {
						var parsedBlocks = _v8.a;
						return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
							$dillonkearns$elm_markdown$Markdown$Block$BlockQuote(parsedBlocks));
					} else {
						var e = _v8.a;
						return $dillonkearns$elm_markdown$Markdown$Parser$InlineProblem(e);
					}
				} else {
					var error = _v7.a;
					return $dillonkearns$elm_markdown$Markdown$Parser$InlineProblem(
						$elm$parser$Parser$Problem(
							$dillonkearns$elm_markdown$Markdown$Parser$deadEndsToString(error)));
				}
			case 'IndentedCodeBlock':
				var codeBlockBody = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$CodeBlock(
						{body: codeBlockBody, language: $elm$core$Maybe$Nothing}));
			case 'Table':
				var _v9 = rawBlock.a;
				var header = _v9.a;
				var rows = _v9.b;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					A2(
						$dillonkearns$elm_markdown$Markdown$Block$Table,
						A2($dillonkearns$elm_markdown$Markdown$Parser$parseHeaderInlines, linkReferences, header),
						A2($dillonkearns$elm_markdown$Markdown$Parser$parseRowInlines, linkReferences, rows)));
			case 'TableDelimiter':
				var _v10 = rawBlock.a;
				var text = _v10.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$Paragraph(
						A2(
							$dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper,
							linkReferences,
							$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(text.raw))));
			default:
				var raw = rawBlock.b;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$Paragraph(
						A2(
							$dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper,
							linkReferences,
							$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(raw))));
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$parseRawInline = F3(
	function (linkReferences, wrap, unparsedInlines) {
		return wrap(
			A2($dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper, linkReferences, unparsedInlines));
	});
var $dillonkearns$elm_markdown$Markdown$Parser$parseRowInlines = F2(
	function (linkReferences, rows) {
		return A2(
			$elm$core$List$map,
			function (row) {
				return A2(
					$elm$core$List$map,
					function (column) {
						return A3(
							$dillonkearns$elm_markdown$Markdown$Parser$parseRawInline,
							linkReferences,
							$elm$core$Basics$identity,
							$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(column));
					},
					row);
			},
			rows);
	});
var $dillonkearns$elm_markdown$Markdown$Parser$stepRawBlock = function (revStmts) {
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v2) {
					return $elm$parser$Parser$Advanced$Done(revStmts);
				},
				$dillonkearns$elm_markdown$Helpers$endOfFile),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (reference) {
					return $elm$parser$Parser$Advanced$Loop(
						A2($dillonkearns$elm_markdown$Markdown$Parser$addReference, revStmts, reference));
				},
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$parser)),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (block) {
					return $elm$parser$Parser$Advanced$Loop(
						A2($dillonkearns$elm_markdown$Markdown$Parser$completeOrMergeBlocks, revStmts, block));
				},
				function () {
					var _v3 = revStmts.rawBlocks;
					_v3$2:
					while (true) {
						if (_v3.b) {
							switch (_v3.a.$) {
								case 'OpenBlockOrParagraph':
									return $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockAfterOpenBlockOrParagraphParser();
								case 'Table':
									var table = _v3.a.a;
									return $elm$parser$Parser$Advanced$oneOf(
										_List_fromArray(
											[
												$dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockNotAfterOpenBlockOrParagraphParser(),
												$dillonkearns$elm_markdown$Markdown$Parser$tableRowIfTableStarted(table)
											]));
								default:
									break _v3$2;
							}
						} else {
							break _v3$2;
						}
					}
					return $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockNotAfterOpenBlockOrParagraphParser();
				}()),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (block) {
					return $elm$parser$Parser$Advanced$Loop(
						A2($dillonkearns$elm_markdown$Markdown$Parser$completeOrMergeBlocks, revStmts, block));
				},
				$dillonkearns$elm_markdown$Markdown$Parser$openBlockOrParagraphParser)
			]));
};
var $dillonkearns$elm_markdown$Markdown$Parser$textNodeToBlocks = function (textNodeValue) {
	return A2(
		$elm$core$Result$withDefault,
		_List_Nil,
		$dillonkearns$elm_markdown$Markdown$Parser$parse(textNodeValue));
};
var $dillonkearns$elm_markdown$Markdown$Parser$xmlNodeToHtmlNode = function (xmlNode) {
	switch (xmlNode.$) {
		case 'Text':
			var innerText = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
					$dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines(innerText)));
		case 'Element':
			var tag = xmlNode.a;
			var attributes = xmlNode.b;
			var children = xmlNode.c;
			var _v1 = $dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocks(children);
			if (_v1.$ === 'Ok') {
				var parsedChildren = _v1.a;
				return $elm$parser$Parser$Advanced$succeed(
					$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
						A3($dillonkearns$elm_markdown$Markdown$Block$HtmlElement, tag, attributes, parsedChildren)));
			} else {
				var err = _v1.a;
				return $elm$parser$Parser$Advanced$problem(err);
			}
		case 'Comment':
			var string = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
					$dillonkearns$elm_markdown$Markdown$Block$HtmlComment(string)));
		case 'Cdata':
			var string = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
					$dillonkearns$elm_markdown$Markdown$Block$Cdata(string)));
		case 'ProcessingInstruction':
			var string = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
					$dillonkearns$elm_markdown$Markdown$Block$ProcessingInstruction(string)));
		default:
			var declarationType = xmlNode.a;
			var content = xmlNode.b;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
					A2($dillonkearns$elm_markdown$Markdown$Block$HtmlDeclaration, declarationType, content)));
	}
};
function $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockNotAfterOpenBlockOrParagraphParser() {
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				$dillonkearns$elm_markdown$Markdown$Parser$parseAsParagraphInsteadOfHtmlBlock,
				$dillonkearns$elm_markdown$Markdown$Parser$blankLine,
				$dillonkearns$elm_markdown$Markdown$Parser$blockQuote,
				A2(
				$elm$parser$Parser$Advanced$map,
				$dillonkearns$elm_markdown$Markdown$RawBlock$CodeBlock,
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$CodeBlock$parser)),
				$dillonkearns$elm_markdown$Markdown$Parser$indentedCodeBlock,
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v26) {
					return $dillonkearns$elm_markdown$Markdown$RawBlock$ThematicBreak;
				},
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$ThematicBreak$parser)),
				$dillonkearns$elm_markdown$Markdown$Parser$unorderedListBlock,
				$dillonkearns$elm_markdown$Markdown$Parser$orderedListBlock(false),
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$Heading$parser),
				$dillonkearns$elm_markdown$Markdown$Parser$cyclic$htmlParser()
			]));
}
function $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockAfterOpenBlockOrParagraphParser() {
	return $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				$dillonkearns$elm_markdown$Markdown$Parser$parseAsParagraphInsteadOfHtmlBlock,
				$dillonkearns$elm_markdown$Markdown$Parser$blankLine,
				$dillonkearns$elm_markdown$Markdown$Parser$blockQuote,
				A2(
				$elm$parser$Parser$Advanced$map,
				$dillonkearns$elm_markdown$Markdown$RawBlock$CodeBlock,
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$CodeBlock$parser)),
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$Parser$setextLineParser),
				A2(
				$elm$parser$Parser$Advanced$map,
				function (_v25) {
					return $dillonkearns$elm_markdown$Markdown$RawBlock$ThematicBreak;
				},
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$ThematicBreak$parser)),
				$dillonkearns$elm_markdown$Markdown$Parser$unorderedListBlock,
				$dillonkearns$elm_markdown$Markdown$Parser$orderedListBlock(true),
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$Heading$parser),
				$dillonkearns$elm_markdown$Markdown$Parser$cyclic$htmlParser(),
				$elm$parser$Parser$Advanced$backtrackable($dillonkearns$elm_markdown$Markdown$Parser$tableDelimiterInOpenParagraph)
			]));
}
function $dillonkearns$elm_markdown$Markdown$Parser$cyclic$htmlParser() {
	return A2($elm$parser$Parser$Advanced$andThen, $dillonkearns$elm_markdown$Markdown$Parser$xmlNodeToHtmlNode, $dillonkearns$elm_markdown$HtmlParser$html);
}
function $dillonkearns$elm_markdown$Markdown$Parser$cyclic$rawBlockParser() {
	return A2(
		$elm$parser$Parser$Advanced$loop,
		{linkReferenceDefinitions: _List_Nil, rawBlocks: _List_Nil},
		$dillonkearns$elm_markdown$Markdown$Parser$stepRawBlock);
}
try {
	var $dillonkearns$elm_markdown$Markdown$Parser$mergeableBlockNotAfterOpenBlockOrParagraphParser = $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockNotAfterOpenBlockOrParagraphParser();
	$dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockNotAfterOpenBlockOrParagraphParser = function () {
		return $dillonkearns$elm_markdown$Markdown$Parser$mergeableBlockNotAfterOpenBlockOrParagraphParser;
	};
	var $dillonkearns$elm_markdown$Markdown$Parser$mergeableBlockAfterOpenBlockOrParagraphParser = $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockAfterOpenBlockOrParagraphParser();
	$dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockAfterOpenBlockOrParagraphParser = function () {
		return $dillonkearns$elm_markdown$Markdown$Parser$mergeableBlockAfterOpenBlockOrParagraphParser;
	};
	var $dillonkearns$elm_markdown$Markdown$Parser$htmlParser = $dillonkearns$elm_markdown$Markdown$Parser$cyclic$htmlParser();
	$dillonkearns$elm_markdown$Markdown$Parser$cyclic$htmlParser = function () {
		return $dillonkearns$elm_markdown$Markdown$Parser$htmlParser;
	};
	var $dillonkearns$elm_markdown$Markdown$Parser$rawBlockParser = $dillonkearns$elm_markdown$Markdown$Parser$cyclic$rawBlockParser();
	$dillonkearns$elm_markdown$Markdown$Parser$cyclic$rawBlockParser = function () {
		return $dillonkearns$elm_markdown$Markdown$Parser$rawBlockParser;
	};
} catch ($) {
	throw 'Some top-level definitions from `Markdown.Parser` are causing infinite recursion:\n\n  ┌─────┐\n  │    childToBlocks\n  │     ↓\n  │    mergeableBlockNotAfterOpenBlockOrParagraphParser\n  │     ↓\n  │    mergeableBlockAfterOpenBlockOrParagraphParser\n  │     ↓\n  │    htmlParser\n  │     ↓\n  │    inlineParseHelper\n  │     ↓\n  │    mapInline\n  │     ↓\n  │    nodeToRawBlock\n  │     ↓\n  │    nodesToBlocks\n  │     ↓\n  │    nodesToBlocksHelp\n  │     ↓\n  │    parse\n  │     ↓\n  │    parseAllInlines\n  │     ↓\n  │    parseAllInlinesHelp\n  │     ↓\n  │    parseHeaderInlines\n  │     ↓\n  │    parseInlines\n  │     ↓\n  │    parseRawInline\n  │     ↓\n  │    parseRowInlines\n  │     ↓\n  │    rawBlockParser\n  │     ↓\n  │    stepRawBlock\n  │     ↓\n  │    textNodeToBlocks\n  │     ↓\n  │    xmlNodeToHtmlNode\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 'Err') {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 'Err') {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return $elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$combineResults = A2(
	$elm$core$List$foldr,
	$elm$core$Result$map2($elm$core$List$cons),
	$elm$core$Result$Ok(_List_Nil));
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Block$foldl = F3(
	function (_function, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var block = list.a;
				var remainingBlocks = list.b;
				switch (block.$) {
					case 'HtmlBlock':
						var html = block.a;
						if (html.$ === 'HtmlElement') {
							var children = html.c;
							var $temp$function = _function,
								$temp$acc = A2(_function, block, acc),
								$temp$list = _Utils_ap(children, remainingBlocks);
							_function = $temp$function;
							acc = $temp$acc;
							list = $temp$list;
							continue foldl;
						} else {
							var $temp$function = _function,
								$temp$acc = A2(_function, block, acc),
								$temp$list = remainingBlocks;
							_function = $temp$function;
							acc = $temp$acc;
							list = $temp$list;
							continue foldl;
						}
					case 'UnorderedList':
						var listItems = block.a;
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 'OrderedList':
						var _int = block.a;
						var lists = block.b;
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 'BlockQuote':
						var blocks = block.a;
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = _Utils_ap(blocks, remainingBlocks);
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 'Heading':
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 'Paragraph':
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 'Table':
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 'CodeBlock':
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					default:
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Block$extractInlineBlockText = function (block) {
	switch (block.$) {
		case 'Paragraph':
			var inlines = block.a;
			return $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines);
		case 'HtmlBlock':
			var html = block.a;
			if (html.$ === 'HtmlElement') {
				var blocks = html.c;
				return A3(
					$dillonkearns$elm_markdown$Markdown$Block$foldl,
					F2(
						function (nestedBlock, soFar) {
							return _Utils_ap(
								soFar,
								$dillonkearns$elm_markdown$Markdown$Block$extractInlineBlockText(nestedBlock));
						}),
					'',
					blocks);
			} else {
				return '';
			}
		case 'UnorderedList':
			var items = block.a;
			return A2(
				$elm$core$String$join,
				'\n',
				A2(
					$elm$core$List$map,
					function (_v4) {
						var task = _v4.a;
						var inlines = _v4.b;
						return $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines);
					},
					items));
		case 'OrderedList':
			var _int = block.a;
			var items = block.b;
			return A2(
				$elm$core$String$join,
				'\n',
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Block$extractInlineText, items));
		case 'BlockQuote':
			var blocks = block.a;
			return A2(
				$elm$core$String$join,
				'\n',
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Block$extractInlineBlockText, blocks));
		case 'Heading':
			var headingLevel = block.a;
			var inlines = block.b;
			return $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines);
		case 'Table':
			var header = block.a;
			var rows = block.b;
			return A2(
				$elm$core$String$join,
				'\n',
				$elm$core$List$concat(
					_List_fromArray(
						[
							A2(
							$elm$core$List$map,
							$dillonkearns$elm_markdown$Markdown$Block$extractInlineText,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.label;
								},
								header)),
							$elm$core$List$concat(
							A2(
								$elm$core$List$map,
								$elm$core$List$map($dillonkearns$elm_markdown$Markdown$Block$extractInlineText),
								rows))
						])));
		case 'CodeBlock':
			var body = block.a.body;
			return body;
		default:
			return '';
	}
};
var $dillonkearns$elm_markdown$Markdown$Block$extractInlineText = function (inlines) {
	return A3($elm$core$List$foldl, $dillonkearns$elm_markdown$Markdown$Block$extractTextHelp, '', inlines);
};
var $dillonkearns$elm_markdown$Markdown$Block$extractTextHelp = F2(
	function (inline, text) {
		switch (inline.$) {
			case 'Text':
				var str = inline.a;
				return _Utils_ap(text, str);
			case 'HardLineBreak':
				return text + ' ';
			case 'CodeSpan':
				var str = inline.a;
				return _Utils_ap(text, str);
			case 'Link':
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			case 'Image':
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			case 'HtmlInline':
				var html = inline.a;
				if (html.$ === 'HtmlElement') {
					var blocks = html.c;
					return A3(
						$dillonkearns$elm_markdown$Markdown$Block$foldl,
						F2(
							function (block, soFar) {
								return _Utils_ap(
									soFar,
									$dillonkearns$elm_markdown$Markdown$Block$extractInlineBlockText(block));
							}),
						text,
						blocks);
				} else {
					return text;
				}
			case 'Strong':
				var inlines = inline.a;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			case 'Emphasis':
				var inlines = inline.a;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			default:
				var inlines = inline.a;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
		}
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$renderHtml = F5(
	function (tagName, attributes, children, _v0, renderedChildren) {
		var htmlRenderer = _v0.a;
		return A2(
			$elm$core$Result$andThen,
			function (okChildren) {
				return A2(
					$elm$core$Result$map,
					function (myRenderer) {
						return myRenderer(okChildren);
					},
					A3(htmlRenderer, tagName, attributes, children));
			},
			$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(renderedChildren));
	});
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $dillonkearns$elm_markdown$Markdown$Renderer$foldThing = F3(
	function (renderer, topLevelInline, soFar) {
		var _v7 = A2($dillonkearns$elm_markdown$Markdown$Renderer$renderSingleInline, renderer, topLevelInline);
		if (_v7.$ === 'Just') {
			var inline = _v7.a;
			return A2($elm$core$List$cons, inline, soFar);
		} else {
			return soFar;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$renderHelper = F2(
	function (renderer, blocks) {
		return A2(
			$elm$core$List$filterMap,
			$dillonkearns$elm_markdown$Markdown$Renderer$renderHelperSingle(renderer),
			blocks);
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$renderHelperSingle = function (renderer) {
	return function (block) {
		switch (block.$) {
			case 'Heading':
				var level = block.a;
				var content = block.b;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						function (children) {
							return renderer.heading(
								{
									children: children,
									level: level,
									rawText: $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(content)
								});
						},
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, content)));
			case 'Paragraph':
				var content = block.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.paragraph,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, content)));
			case 'HtmlBlock':
				var html = block.a;
				if (html.$ === 'HtmlElement') {
					var tag = html.a;
					var attributes = html.b;
					var children = html.c;
					return $elm$core$Maybe$Just(
						A4($dillonkearns$elm_markdown$Markdown$Renderer$renderHtmlNode, renderer, tag, attributes, children));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			case 'UnorderedList':
				var items = block.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.unorderedList,
						$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
							A2(
								$elm$core$List$map,
								function (_v4) {
									var task = _v4.a;
									var children = _v4.b;
									return A2(
										$elm$core$Result$map,
										function (renderedBody) {
											return A2($dillonkearns$elm_markdown$Markdown$Block$ListItem, task, renderedBody);
										},
										A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, children));
								},
								items))));
			case 'OrderedList':
				var startingIndex = block.a;
				var items = block.b;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.orderedList(startingIndex),
						$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
							A2(
								$elm$core$List$map,
								$dillonkearns$elm_markdown$Markdown$Renderer$renderStyled(renderer),
								items))));
			case 'CodeBlock':
				var codeBlock = block.a;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.codeBlock(codeBlock)));
			case 'ThematicBreak':
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(renderer.thematicBreak));
			case 'BlockQuote':
				var nestedBlocks = block.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.blockQuote,
						$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
							A2($dillonkearns$elm_markdown$Markdown$Renderer$renderHelper, renderer, nestedBlocks))));
			default:
				var header = block.a;
				var rows = block.b;
				var renderedHeaderCells = $dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
					A2(
						$elm$core$List$map,
						function (_v6) {
							var label = _v6.label;
							var alignment = _v6.alignment;
							return A2(
								$elm$core$Result$map,
								$elm$core$Tuple$pair(alignment),
								A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, label));
						},
						header));
				var renderedHeader = A2(
					$elm$core$Result$map,
					function (listListView) {
						return renderer.tableHeader(
							$elm$core$List$singleton(
								renderer.tableRow(
									A2(
										$elm$core$List$map,
										function (_v5) {
											var maybeAlignment = _v5.a;
											var item = _v5.b;
											return A2(renderer.tableHeaderCell, maybeAlignment, item);
										},
										listListView))));
					},
					renderedHeaderCells);
				var renderedBody = function (r) {
					return $elm$core$List$isEmpty(r) ? _List_Nil : _List_fromArray(
						[
							renderer.tableBody(r)
						]);
				};
				var alignmentForColumn = function (columnIndex) {
					return A2(
						$elm$core$Maybe$andThen,
						function ($) {
							return $.alignment;
						},
						$elm$core$List$head(
							A2($elm$core$List$drop, columnIndex, header)));
				};
				var renderRow = function (cells) {
					return A2(
						$elm$core$Result$map,
						renderer.tableRow,
						A2(
							$elm$core$Result$map,
							$elm$core$List$indexedMap(
								F2(
									function (index, cell) {
										return A2(
											renderer.tableCell,
											alignmentForColumn(index),
											cell);
									})),
							$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
								A2(
									$elm$core$List$map,
									$dillonkearns$elm_markdown$Markdown$Renderer$renderStyled(renderer),
									cells))));
				};
				var renderedRows = $dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
					A2($elm$core$List$map, renderRow, rows));
				return $elm$core$Maybe$Just(
					A3(
						$elm$core$Result$map2,
						F2(
							function (h, r) {
								return renderer.table(
									A2(
										$elm$core$List$cons,
										h,
										renderedBody(r)));
							}),
						renderedHeader,
						renderedRows));
		}
	};
};
var $dillonkearns$elm_markdown$Markdown$Renderer$renderHtmlNode = F4(
	function (renderer, tag, attributes, children) {
		return A5(
			$dillonkearns$elm_markdown$Markdown$Renderer$renderHtml,
			tag,
			attributes,
			children,
			renderer.html,
			A2($dillonkearns$elm_markdown$Markdown$Renderer$renderHelper, renderer, children));
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$renderSingleInline = F2(
	function (renderer, inline) {
		switch (inline.$) {
			case 'Strong':
				var innerInlines = inline.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.strong,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, innerInlines)));
			case 'Emphasis':
				var innerInlines = inline.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.emphasis,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, innerInlines)));
			case 'Strikethrough':
				var innerInlines = inline.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.strikethrough,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, innerInlines)));
			case 'Image':
				var src = inline.a;
				var title = inline.b;
				var children = inline.c;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.image(
							{
								alt: $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(children),
								src: src,
								title: title
							})));
			case 'Text':
				var string = inline.a;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.text(string)));
			case 'CodeSpan':
				var string = inline.a;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.codeSpan(string)));
			case 'Link':
				var destination = inline.a;
				var title = inline.b;
				var inlines = inline.c;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$andThen,
						function (children) {
							return $elm$core$Result$Ok(
								A2(
									renderer.link,
									{destination: destination, title: title},
									children));
						},
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, inlines)));
			case 'HardLineBreak':
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(renderer.hardLineBreak));
			default:
				var html = inline.a;
				if (html.$ === 'HtmlElement') {
					var tag = html.a;
					var attributes = html.b;
					var children = html.c;
					return $elm$core$Maybe$Just(
						A4($dillonkearns$elm_markdown$Markdown$Renderer$renderHtmlNode, renderer, tag, attributes, children));
				} else {
					return $elm$core$Maybe$Nothing;
				}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$renderStyled = F2(
	function (renderer, styledStrings) {
		return $dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
			A3(
				$elm$core$List$foldr,
				$dillonkearns$elm_markdown$Markdown$Renderer$foldThing(renderer),
				_List_Nil,
				styledStrings));
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$render = F2(
	function (renderer, ast) {
		return $dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
			A2($dillonkearns$elm_markdown$Markdown$Renderer$renderHelper, renderer, ast));
	});
var $mdgriffith$elm_ui$Internal$Model$Top = {$: 'Top'};
var $mdgriffith$elm_ui$Element$alignTop = $mdgriffith$elm_ui$Internal$Model$AlignY($mdgriffith$elm_ui$Internal$Model$Top);
var $elm$html$Html$br = _VirtualDom_node('br');
var $mdgriffith$elm_ui$Internal$Flag$fontAlignment = $mdgriffith$elm_ui$Internal$Flag$flag(12);
var $mdgriffith$elm_ui$Element$Font$center = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.textCenter);
var $mdgriffith$elm_ui$Internal$Model$ImportFont = F2(
	function (a, b) {
		return {$: 'ImportFont', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$Font$external = function (_v0) {
	var url = _v0.url;
	var name = _v0.name;
	return A2($mdgriffith$elm_ui$Internal$Model$ImportFont, name, url);
};
var $mdgriffith$elm_ui$Element$Font$family = function (families) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontFamily,
		A2(
			$mdgriffith$elm_ui$Internal$Model$FontFamily,
			A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'ff-', families),
			families));
};
var $mdgriffith$elm_ui$Element$rgba = $mdgriffith$elm_ui$Internal$Model$Rgba;
var $mdgriffith$elm_ui$Internal$Flag$borderRound = $mdgriffith$elm_ui$Internal$Flag$flag(17);
var $mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + $elm$core$String$fromInt(radius),
			'border-radius',
			$elm$core$String$fromInt(radius) + 'px'));
};
var $author$project$Utils$Markdown$code = function (snippet) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Background$color(
				A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0.04)),
				$mdgriffith$elm_ui$Element$Border$rounded(2),
				A2($mdgriffith$elm_ui$Element$paddingXY, 5, 3),
				$mdgriffith$elm_ui$Element$Font$family(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$external(
						{name: 'Source Code Pro', url: 'https://fonts.googleapis.com/css?family=Source+Code+Pro'})
					]))
			]),
		$mdgriffith$elm_ui$Element$text(snippet));
};
var $mdgriffith$elm_ui$Element$padding = function (x) {
	var f = x;
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + $elm$core$String$fromInt(x),
			f,
			f,
			f,
			f));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Utils$Markdown$codeBlock = function (details) {
	return A2(
		$mdgriffith$elm_ui$Element$paragraph,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Background$color(
				A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0.03)),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				A2($elm$html$Html$Attributes$style, 'white-space', 'pre')),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'break-word')),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				A2($elm$html$Html$Attributes$style, 'word-break', 'break-word')),
				$mdgriffith$elm_ui$Element$padding(20),
				$mdgriffith$elm_ui$Element$Font$family(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$external(
						{name: 'Source Code Pro', url: 'https://fonts.googleapis.com/css?family=Source+Code+Pro'})
					]))
			]),
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$text(details.body)
			]));
};
var $mdgriffith$elm_ui$Internal$Flag$borderColor = $mdgriffith$elm_ui$Internal$Flag$flag(28);
var $mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $mdgriffith$elm_ui$Internal$Model$InFront = {$: 'InFront'};
var $mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 'Nearby', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$createNearby = F2(
	function (loc, element) {
		if (element.$ === 'Empty') {
			return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
		} else {
			return A2($mdgriffith$elm_ui$Internal$Model$Nearby, loc, element);
		}
	});
var $mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, $mdgriffith$elm_ui$Internal$Model$InFront, element);
};
var $mdgriffith$elm_ui$Internal$Model$MoveY = function (a) {
	return {$: 'MoveY', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 'TransformComponent', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$moveY = $mdgriffith$elm_ui$Internal$Flag$flag(26);
var $mdgriffith$elm_ui$Element$moveUp = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(-y));
};
var $mdgriffith$elm_ui$Internal$Model$Empty = {$: 'Empty'};
var $mdgriffith$elm_ui$Element$none = $mdgriffith$elm_ui$Internal$Model$Empty;
var $mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 'Px', a: a};
};
var $mdgriffith$elm_ui$Element$px = $mdgriffith$elm_ui$Internal$Model$Px;
var $mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var $mdgriffith$elm_ui$Internal$Model$Rotate = F2(
	function (a, b) {
		return {$: 'Rotate', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$rotate = $mdgriffith$elm_ui$Internal$Flag$flag(24);
var $mdgriffith$elm_ui$Element$rotate = function (angle) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$rotate,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Rotate,
			_Utils_Tuple3(0, 0, 1),
			angle));
};
var $mdgriffith$elm_ui$Internal$Model$boxShadowClass = function (shadow) {
	return $elm$core$String$concat(
		_List_fromArray(
			[
				shadow.inset ? 'box-inset' : 'box-',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.offset.a) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.offset.b) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.blur) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.size) + 'px',
				$mdgriffith$elm_ui$Internal$Model$formatColorClass(shadow.color)
			]));
};
var $mdgriffith$elm_ui$Internal$Flag$shadows = $mdgriffith$elm_ui$Internal$Flag$flag(19);
var $mdgriffith$elm_ui$Element$Border$shadow = function (almostShade) {
	var shade = {blur: almostShade.blur, color: almostShade.color, inset: false, offset: almostShade.offset, size: almostShade.size};
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$shadows,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			$mdgriffith$elm_ui$Internal$Model$boxShadowClass(shade),
			'box-shadow',
			$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(shade)));
};
var $mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontSize,
		$mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var $mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 'Transparency', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$transparency = $mdgriffith$elm_ui$Internal$Flag$flag(0);
var $mdgriffith$elm_ui$Element$transparent = function (on) {
	return on ? A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2($mdgriffith$elm_ui$Internal$Model$Transparency, 'transparent', 1.0)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2($mdgriffith$elm_ui$Internal$Model$Transparency, 'visible', 0.0));
};
var $mdgriffith$elm_ui$Element$Input$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 'BorderWidth', a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + $elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var $mdgriffith$elm_ui$Element$Border$widthXY = F2(
	function (x, y) {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$borderWidth,
			A5(
				$mdgriffith$elm_ui$Internal$Model$BorderWidth,
				'b-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var $mdgriffith$elm_ui$Element$Border$widthEach = function (_v0) {
	var bottom = _v0.bottom;
	var top = _v0.top;
	var left = _v0.left;
	var right = _v0.right;
	return (_Utils_eq(top, bottom) && _Utils_eq(left, right)) ? (_Utils_eq(top, right) ? $mdgriffith$elm_ui$Element$Border$width(top) : A2($mdgriffith$elm_ui$Element$Border$widthXY, left, top)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left))))))),
			top,
			right,
			bottom,
			left));
};
var $mdgriffith$elm_ui$Element$Input$defaultCheckbox = function (checked) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Internal$Model$htmlClass('focusable'),
				$mdgriffith$elm_ui$Element$width(
				$mdgriffith$elm_ui$Element$px(14)),
				$mdgriffith$elm_ui$Element$height(
				$mdgriffith$elm_ui$Element$px(14)),
				$mdgriffith$elm_ui$Element$Font$color($mdgriffith$elm_ui$Element$Input$white),
				$mdgriffith$elm_ui$Element$centerY,
				$mdgriffith$elm_ui$Element$Font$size(9),
				$mdgriffith$elm_ui$Element$Font$center,
				$mdgriffith$elm_ui$Element$Border$rounded(3),
				$mdgriffith$elm_ui$Element$Border$color(
				checked ? A3($mdgriffith$elm_ui$Element$rgb, 59 / 255, 153 / 255, 252 / 255) : A3($mdgriffith$elm_ui$Element$rgb, 211 / 255, 211 / 255, 211 / 255)),
				$mdgriffith$elm_ui$Element$Border$shadow(
				{
					blur: 1,
					color: checked ? A4($mdgriffith$elm_ui$Element$rgba, 238 / 255, 238 / 255, 238 / 255, 0) : A3($mdgriffith$elm_ui$Element$rgb, 238 / 255, 238 / 255, 238 / 255),
					offset: _Utils_Tuple2(0, 0),
					size: 1
				}),
				$mdgriffith$elm_ui$Element$Background$color(
				checked ? A3($mdgriffith$elm_ui$Element$rgb, 59 / 255, 153 / 255, 252 / 255) : $mdgriffith$elm_ui$Element$Input$white),
				$mdgriffith$elm_ui$Element$Border$width(
				checked ? 0 : 1),
				$mdgriffith$elm_ui$Element$inFront(
				A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Border$color($mdgriffith$elm_ui$Element$Input$white),
							$mdgriffith$elm_ui$Element$height(
							$mdgriffith$elm_ui$Element$px(6)),
							$mdgriffith$elm_ui$Element$width(
							$mdgriffith$elm_ui$Element$px(9)),
							$mdgriffith$elm_ui$Element$rotate(
							$elm$core$Basics$degrees(-45)),
							$mdgriffith$elm_ui$Element$centerX,
							$mdgriffith$elm_ui$Element$centerY,
							$mdgriffith$elm_ui$Element$moveUp(1),
							$mdgriffith$elm_ui$Element$transparent(!checked),
							$mdgriffith$elm_ui$Element$Border$widthEach(
							{bottom: 2, left: 2, right: 0, top: 0})
						]),
					$mdgriffith$elm_ui$Element$none))
			]),
		$mdgriffith$elm_ui$Element$none);
};
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $mdgriffith$elm_ui$Internal$Model$Heading = function (a) {
	return {$: 'Heading', a: a};
};
var $mdgriffith$elm_ui$Element$Region$heading = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Describe, $mdgriffith$elm_ui$Internal$Model$Heading);
var $dillonkearns$elm_markdown$Markdown$Block$headingLevelToInt = function (headingLevel) {
	switch (headingLevel.$) {
		case 'H1':
			return 1;
		case 'H2':
			return 2;
		case 'H3':
			return 3;
		case 'H4':
			return 4;
		case 'H5':
			return 5;
		default:
			return 6;
	}
};
var $author$project$Utils$Markdown$rawTextToId = function (rawText) {
	return $elm$core$String$toLower(
		A2(
			$elm$core$String$join,
			'-',
			A2($elm$core$String$split, ' ', rawText)));
};
var $mdgriffith$elm_ui$Element$Font$typeface = $mdgriffith$elm_ui$Internal$Model$Typeface;
var $author$project$Utils$Markdown$heading = function (_v0) {
	var level = _v0.level;
	var rawText = _v0.rawText;
	var children = _v0.children;
	return A2(
		$mdgriffith$elm_ui$Element$paragraph,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Font$size(
				function () {
					switch (level.$) {
						case 'H1':
							return 36;
						case 'H2':
							return 24;
						default:
							return 20;
					}
				}()),
				$mdgriffith$elm_ui$Element$Font$bold,
				$mdgriffith$elm_ui$Element$Font$family(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$typeface('Montserrat')
					])),
				$mdgriffith$elm_ui$Element$Region$heading(
				$dillonkearns$elm_markdown$Markdown$Block$headingLevelToInt(level)),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				A2(
					$elm$html$Html$Attributes$attribute,
					'name',
					$author$project$Utils$Markdown$rawTextToId(rawText))),
				$mdgriffith$elm_ui$Element$htmlAttribute(
				$elm$html$Html$Attributes$id(
					$author$project$Utils$Markdown$rawTextToId(rawText)))
			]),
		children);
};
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $mdgriffith$elm_ui$Element$html = $mdgriffith$elm_ui$Internal$Model$unstyled;
var $elm$html$Html$Attributes$alt = $elm$html$Html$Attributes$stringProperty('alt');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $mdgriffith$elm_ui$Element$image = F2(
	function (attrs, _v0) {
		var src = _v0.src;
		var description = _v0.description;
		var imageAttributes = A2(
			$elm$core$List$filter,
			function (a) {
				switch (a.$) {
					case 'Width':
						return true;
					case 'Height':
						return true;
					default:
						return false;
				}
			},
			attrs);
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.imageContainer),
				attrs),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[
						A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asEl,
						$mdgriffith$elm_ui$Internal$Model$NodeName('img'),
						_Utils_ap(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Internal$Model$Attr(
									$elm$html$Html$Attributes$src(src)),
									$mdgriffith$elm_ui$Internal$Model$Attr(
									$elm$html$Html$Attributes$alt(description))
								]),
							imageAttributes),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil))
					])));
	});
var $mdgriffith$elm_ui$Element$Font$italic = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.italic);
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $mdgriffith$elm_ui$Element$newTabLink = F2(
	function (attrs, _v0) {
		var url = _v0.url;
		var label = _v0.label;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$NodeName('a'),
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Attr(
					$elm$html$Html$Attributes$href(url)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Internal$Model$Attr(
						$elm$html$Html$Attributes$rel('noopener noreferrer')),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Attributes$target('_blank')),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentCenterX + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.contentCenterY + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.link)))),
									attrs)))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $dillonkearns$elm_markdown$Markdown$HtmlRenderer$HtmlRenderer = function (a) {
	return {$: 'HtmlRenderer', a: a};
};
var $dillonkearns$elm_markdown$Markdown$Html$resultOr = F2(
	function (ra, rb) {
		if (ra.$ === 'Err') {
			var singleError = ra.a;
			if (rb.$ === 'Ok') {
				var okValue = rb.a;
				return $elm$core$Result$Ok(okValue);
			} else {
				var errorsSoFar = rb.a;
				return $elm$core$Result$Err(
					A2($elm$core$List$cons, singleError, errorsSoFar));
			}
		} else {
			var okValue = ra.a;
			return $elm$core$Result$Ok(okValue);
		}
	});
var $dillonkearns$elm_markdown$Markdown$Html$attributesToString = function (attributes) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$map,
			function (_v0) {
				var name = _v0.name;
				var value = _v0.value;
				return name + ('=\"' + (value + '\"'));
			},
			attributes));
};
var $dillonkearns$elm_markdown$Markdown$Html$tagToString = F2(
	function (tagName, attributes) {
		return $elm$core$List$isEmpty(attributes) ? ('<' + (tagName + '>')) : ('<' + (tagName + (' ' + ($dillonkearns$elm_markdown$Markdown$Html$attributesToString(attributes) + '>'))));
	});
var $dillonkearns$elm_markdown$Markdown$Html$oneOf = function (decoders) {
	var unwrappedDecoders = A2(
		$elm$core$List$map,
		function (_v1) {
			var rawDecoder = _v1.a;
			return rawDecoder;
		},
		decoders);
	return function (rawDecoder) {
		return $dillonkearns$elm_markdown$Markdown$HtmlRenderer$HtmlRenderer(
			F3(
				function (tagName, attributes, innerBlocks) {
					return A2(
						$elm$core$Result$mapError,
						function (errors) {
							if (!errors.b) {
								return 'Ran into a oneOf with no possibilities!';
							} else {
								if (!errors.b.b) {
									var singleError = errors.a;
									return 'Problem with the given value:\n\n' + (A2($dillonkearns$elm_markdown$Markdown$Html$tagToString, tagName, attributes) + ('\n\n' + (singleError + '\n')));
								} else {
									return 'oneOf failed parsing this value:\n    ' + (A2($dillonkearns$elm_markdown$Markdown$Html$tagToString, tagName, attributes) + ('\n\nParsing failed in the following 2 ways:\n\n\n' + (A2(
										$elm$core$String$join,
										'\n\n',
										A2(
											$elm$core$List$indexedMap,
											F2(
												function (index, error) {
													return '(' + ($elm$core$String$fromInt(index + 1) + (') ' + error));
												}),
											errors)) + '\n')));
								}
							}
						},
						A3(rawDecoder, tagName, attributes, innerBlocks));
				}));
	}(
		A3(
			$elm$core$List$foldl,
			F2(
				function (decoder, soFar) {
					return F3(
						function (tagName, attributes, children) {
							return A2(
								$dillonkearns$elm_markdown$Markdown$Html$resultOr,
								A3(decoder, tagName, attributes, children),
								A3(soFar, tagName, attributes, children));
						});
				}),
			F3(
				function (tagName, attributes, children) {
					return $elm$core$Result$Err(_List_Nil);
				}),
			unwrappedDecoders));
};
var $mdgriffith$elm_ui$Internal$Model$AsRow = {$: 'AsRow'};
var $mdgriffith$elm_ui$Internal$Model$asRow = $mdgriffith$elm_ui$Internal$Model$AsRow;
var $mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asRow,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentLeft + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.contentCenterY)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Element$Font$strike = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.strike);
var $mdgriffith$elm_ui$Internal$Flag$borderStyle = $mdgriffith$elm_ui$Internal$Flag$flag(11);
var $mdgriffith$elm_ui$Element$Border$solid = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$borderStyle, $mdgriffith$elm_ui$Internal$Style$classes.borderSolid);
var $author$project$Utils$Markdown$tableBorder = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Border$color(
		A3($mdgriffith$elm_ui$Element$rgb255, 223, 226, 229)),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$Border$solid,
		A2($mdgriffith$elm_ui$Element$paddingXY, 6, 13),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
	]);
var $mdgriffith$elm_ui$Element$Font$underline = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.underline);
var $author$project$Utils$Markdown$renderer = {
	blockQuote: function (children) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Border$widthEach(
					{bottom: 0, left: 10, right: 0, top: 0}),
					$mdgriffith$elm_ui$Element$padding(10),
					$mdgriffith$elm_ui$Element$Border$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 145, 145, 145)),
					$mdgriffith$elm_ui$Element$Background$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 245, 245, 245))
				]),
			children);
	},
	codeBlock: $author$project$Utils$Markdown$codeBlock,
	codeSpan: $author$project$Utils$Markdown$code,
	emphasis: function (content) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$Font$italic]),
			content);
	},
	hardLineBreak: $mdgriffith$elm_ui$Element$html(
		A2($elm$html$Html$br, _List_Nil, _List_Nil)),
	heading: $author$project$Utils$Markdown$heading,
	html: $dillonkearns$elm_markdown$Markdown$Html$oneOf(_List_Nil),
	image: function (image) {
		var _v0 = image.title;
		if (_v0.$ === 'Just') {
			var title = _v0.a;
			return A2(
				$mdgriffith$elm_ui$Element$image,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
					]),
				{description: image.alt, src: image.src});
		} else {
			return A2(
				$mdgriffith$elm_ui$Element$image,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
					]),
				{description: image.alt, src: image.src});
		}
	},
	link: F2(
		function (_v1, body) {
			var title = _v1.title;
			var destination = _v1.destination;
			return A2(
				$mdgriffith$elm_ui$Element$newTabLink,
				_List_Nil,
				{
					label: A2(
						$mdgriffith$elm_ui$Element$paragraph,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$Font$underline,
								$mdgriffith$elm_ui$Element$htmlAttribute(
								A2($elm$html$Html$Attributes$style, 'overflow-wrap', 'break-word')),
								$mdgriffith$elm_ui$Element$htmlAttribute(
								A2($elm$html$Html$Attributes$style, 'word-break', 'break-word'))
							]),
						body),
					url: destination
				});
		}),
	orderedList: F2(
		function (startingIndex, items) {
			return A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$spacing(15)
					]),
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (index, itemBlocks) {
							return A2(
								$mdgriffith$elm_ui$Element$paragraph,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$spacing(5)
									]),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Element$paragraph,
										_List_fromArray(
											[$mdgriffith$elm_ui$Element$alignTop]),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Element$text(
												$elm$core$String$fromInt(index + startingIndex) + ' '),
											itemBlocks))
									]));
						}),
					items));
		}),
	paragraph: $mdgriffith$elm_ui$Element$paragraph(
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 15)
			])),
	strikethrough: function (content) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$Font$strike]),
			content);
	},
	strong: function (content) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$Font$bold]),
			content);
	},
	table: $mdgriffith$elm_ui$Element$column(_List_Nil),
	tableBody: $mdgriffith$elm_ui$Element$column(_List_Nil),
	tableCell: F2(
		function (maybeAlignment, children) {
			return A2($mdgriffith$elm_ui$Element$paragraph, $author$project$Utils$Markdown$tableBorder, children);
		}),
	tableHeader: $mdgriffith$elm_ui$Element$column(
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Font$bold,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$Font$center
			])),
	tableHeaderCell: F2(
		function (maybeAlignment, children) {
			return A2($mdgriffith$elm_ui$Element$paragraph, $author$project$Utils$Markdown$tableBorder, children);
		}),
	tableRow: $mdgriffith$elm_ui$Element$row(
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
			])),
	text: function (value) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_Nil,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$text(value)
				]));
	},
	thematicBreak: $mdgriffith$elm_ui$Element$none,
	unorderedList: function (items) {
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(15)
				]),
			A2(
				$elm$core$List$map,
				function (_v2) {
					var task = _v2.a;
					var children = _v2.b;
					return A2(
						$mdgriffith$elm_ui$Element$paragraph,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$spacing(5)
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$paragraph,
								_List_fromArray(
									[$mdgriffith$elm_ui$Element$alignTop]),
								A2(
									$elm$core$List$cons,
									function () {
										switch (task.$) {
											case 'IncompleteTask':
												return $mdgriffith$elm_ui$Element$Input$defaultCheckbox(false);
											case 'CompletedTask':
												return $mdgriffith$elm_ui$Element$Input$defaultCheckbox(true);
											default:
												return $mdgriffith$elm_ui$Element$text('•');
										}
									}(),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Element$text(' '),
										children)))
							]));
				},
				items));
	}
};
var $author$project$Utils$Markdown$render = function (str) {
	var result = A2(
		$elm$core$Result$andThen,
		$dillonkearns$elm_markdown$Markdown$Renderer$render($author$project$Utils$Markdown$renderer),
		A2(
			$elm$core$Result$mapError,
			function (error) {
				return A2(
					$elm$core$String$join,
					'\n',
					A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$deadEndToString, error));
			},
			$dillonkearns$elm_markdown$Markdown$Parser$parse(str)));
	if (result.$ === 'Err') {
		var errStr = result.a;
		return _List_fromArray(
			[
				$author$project$Common$Contents$plainPara(errStr)
			]);
	} else {
		var rendered = result.a;
		return rendered;
	}
};
var $author$project$Utils$Utils$monthToInt = function (month) {
	switch (month.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$time$Time$Apr = {$: 'Apr'};
var $elm$time$Time$Aug = {$: 'Aug'};
var $elm$time$Time$Dec = {$: 'Dec'};
var $elm$time$Time$Feb = {$: 'Feb'};
var $elm$time$Time$Jan = {$: 'Jan'};
var $elm$time$Time$Jul = {$: 'Jul'};
var $elm$time$Time$Jun = {$: 'Jun'};
var $elm$time$Time$Mar = {$: 'Mar'};
var $elm$time$Time$May = {$: 'May'};
var $elm$time$Time$Nov = {$: 'Nov'};
var $elm$time$Time$Oct = {$: 'Oct'};
var $elm$time$Time$Sep = {$: 'Sep'};
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_v0) {
			case 1:
				return $elm$time$Time$Jan;
			case 2:
				return $elm$time$Time$Feb;
			case 3:
				return $elm$time$Time$Mar;
			case 4:
				return $elm$time$Time$Apr;
			case 5:
				return $elm$time$Time$May;
			case 6:
				return $elm$time$Time$Jun;
			case 7:
				return $elm$time$Time$Jul;
			case 8:
				return $elm$time$Time$Aug;
			case 9:
				return $elm$time$Time$Sep;
			case 10:
				return $elm$time$Time$Oct;
			case 11:
				return $elm$time$Time$Nov;
			default:
				return $elm$time$Time$Dec;
		}
	});
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $author$project$Utils$Utils$formatTime = F2(
	function (targetPosix, currentPosix) {
		var year = A2($elm$time$Time$toYear, $elm$time$Time$utc, targetPosix);
		var sec = A2($elm$time$Time$toSecond, $elm$time$Time$utc, targetPosix);
		var month = $author$project$Utils$Utils$monthToInt(
			A2($elm$time$Time$toMonth, $elm$time$Time$utc, targetPosix));
		var min = A2($elm$time$Time$toMinute, $elm$time$Time$utc, targetPosix);
		var hour = A2($elm$time$Time$toHour, $elm$time$Time$utc, targetPosix);
		var time = A2(
			$elm$core$String$join,
			':',
			A2(
				$elm$core$List$map,
				$elm$core$String$fromInt,
				_List_fromArray(
					[hour, min, sec])));
		var day = A2($elm$time$Time$toDay, $elm$time$Time$utc, targetPosix);
		var date = A2(
			$elm$core$String$join,
			'/',
			A2(
				$elm$core$List$map,
				$elm$core$String$fromInt,
				_List_fromArray(
					[year, month, day])));
		var whole = A2(
			$elm$core$String$join,
			' - ',
			_List_fromArray(
				[date, time]));
		return whole;
	});
var $author$project$Common$Colors$grey = A3($mdgriffith$elm_ui$Element$rgb255, 128, 128, 128);
var $author$project$Common$Contents$timeText = F2(
	function (targetPosix, currentPosix) {
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Font$size(16),
					$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$grey)
				]),
			$mdgriffith$elm_ui$Element$text(
				A2($author$project$Utils$Utils$formatTime, targetPosix, currentPosix)));
	});
var $author$project$Common$Colors$yellow = A3($mdgriffith$elm_ui$Element$rgb255, 220, 220, 20);
var $author$project$Views$Chat$msgView = function (msg) {
	var paddedTimeText = A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$paddingEach(
				{bottom: 0, left: 10, right: 0, top: 0})
			]),
		A2(
			$author$project$Common$Contents$timeText,
			$author$project$Utils$Utils$posixSecToPosix(msg.posixTimeSec),
			$author$project$Utils$Utils$posixSecToPosix(msg.posixTimeSec)));
	var msgFromClient = msg.msgFromClient;
	var _v0 = msgFromClient.msgType;
	switch (_v0.$) {
		case 'Join':
			return A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$green)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text(msg.username + ' joined.'),
						paddedTimeText
					]));
		case 'NameChange':
			return A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$yellow)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text(
						$elm_community$string_extra$String$Extra$quote(msg.username) + (' changed their name to ' + ($elm_community$string_extra$String$Extra$quote(
							$joneshf$elm_tagged$Tagged$untag(msgFromClient.msgBody)) + '.'))),
						paddedTimeText
					]));
		case 'Leave':
			return A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$red)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text(msg.username + ' left.'),
						paddedTimeText
					]));
		default:
			return A2(
				$mdgriffith$elm_ui$Element$column,
				_List_Nil,
				$author$project$Utils$Markdown$render(
					$joneshf$elm_tagged$Tagged$untag(msgFromClient.msgBody)));
	}
};
var $author$project$Views$Chat$msgBundleView = function (bundle) {
	return A2(
		$mdgriffith$elm_ui$Element$textColumn,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10)
			]),
		_Utils_ap(
			_List_fromArray(
				[
					function () {
					var _v0 = $author$project$Chat$isMetaBundle(bundle);
					if (!_v0) {
						return A2(
							$mdgriffith$elm_ui$Element$row,
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Element$spacingXY, 10, 0)
								]),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Element$paragraph,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
											$mdgriffith$elm_ui$Element$Font$bold
										]),
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$text(bundle.username)
										])),
									A2($author$project$Common$Contents$timeText, bundle.time, bundle.time)
								]));
					} else {
						return $mdgriffith$elm_ui$Element$none;
					}
				}()
				]),
			A2($elm$core$List$map, $author$project$Views$Chat$msgView, bundle.msgs)));
};
var $mdgriffith$elm_ui$Element$Input$TextArea = {$: 'TextArea'};
var $mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 'LivePolite'};
var $mdgriffith$elm_ui$Element$Region$announce = $mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$LivePolite);
var $mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 'HiddenLabel') {
			var labelText = label.a;
			return A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asColumn,
				$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
				attrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[input])));
		} else {
			var position = label.a;
			var labelAttrs = label.b;
			var labelChild = label.c;
			var labelElement = A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				labelAttrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[labelChild])));
			switch (position.$) {
				case 'Above':
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 'Below':
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 'OnRight':
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				default:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var $mdgriffith$elm_ui$Element$Input$autofill = A2(
	$elm$core$Basics$composeL,
	$mdgriffith$elm_ui$Internal$Model$Attr,
	$elm$html$Html$Attributes$attribute('autocomplete'));
var $mdgriffith$elm_ui$Internal$Model$Behind = {$: 'Behind'};
var $mdgriffith$elm_ui$Element$behindContent = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, $mdgriffith$elm_ui$Internal$Model$Behind, element);
};
var $mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding = function (attrs) {
	var gatherSpacing = F2(
		function (attr, found) {
			if ((attr.$ === 'StyleClass') && (attr.b.$ === 'SpacingStyle')) {
				var _v2 = attr.b;
				var x = _v2.b;
				var y = _v2.c;
				if (found.$ === 'Nothing') {
					return $elm$core$Maybe$Just(y);
				} else {
					return found;
				}
			} else {
				return found;
			}
		});
	var _v0 = A3($elm$core$List$foldr, gatherSpacing, $elm$core$Maybe$Nothing, attrs);
	if (_v0.$ === 'Nothing') {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	} else {
		var vSpace = _v0.a;
		return $mdgriffith$elm_ui$Element$moveUp(
			$elm$core$Basics$floor(vSpace / 2));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$overflow = $mdgriffith$elm_ui$Internal$Flag$flag(20);
var $mdgriffith$elm_ui$Element$clip = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.clip);
var $mdgriffith$elm_ui$Element$Input$darkGrey = A3($mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var $mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2($mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var $mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Input$defaultTextPadding,
		$mdgriffith$elm_ui$Element$Border$rounded(3),
		$mdgriffith$elm_ui$Element$Border$color($mdgriffith$elm_ui$Element$Input$darkGrey),
		$mdgriffith$elm_ui$Element$Background$color($mdgriffith$elm_ui$Element$Input$white),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$spacing(5),
		$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink)
	]);
var $mdgriffith$elm_ui$Element$Input$getHeight = function (attr) {
	if (attr.$ === 'Height') {
		var h = attr.a;
		return $elm$core$Maybe$Just(h);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 'Label', a: a};
};
var $mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 'HiddenLabel') {
		var textLabel = label.a;
		return $mdgriffith$elm_ui$Internal$Model$Describe(
			$mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var $mdgriffith$elm_ui$Element$Input$isConstrained = function (len) {
	isConstrained:
	while (true) {
		switch (len.$) {
			case 'Content':
				return false;
			case 'Px':
				return true;
			case 'Fill':
				return true;
			case 'Min':
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isConstrained;
			default:
				var l = len.b;
				return true;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isHiddenLabel = function (label) {
	if (label.$ === 'HiddenLabel') {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$isStacked = function (label) {
	if (label.$ === 'Label') {
		var loc = label.a;
		switch (loc.$) {
			case 'OnRight':
				return false;
			case 'OnLeft':
				return false;
			case 'Above':
				return true;
			default:
				return true;
		}
	} else {
		return true;
	}
};
var $mdgriffith$elm_ui$Element$Input$negateBox = function (box) {
	return {bottom: -box.bottom, left: -box.left, right: -box.right, top: -box.top};
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $mdgriffith$elm_ui$Element$Input$isFill = function (len) {
	isFill:
	while (true) {
		switch (len.$) {
			case 'Fill':
				return true;
			case 'Content':
				return false;
			case 'Px':
				return false;
			case 'Min':
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isPixel = function (len) {
	isPixel:
	while (true) {
		switch (len.$) {
			case 'Content':
				return false;
			case 'Px':
				return true;
			case 'Fill':
				return false;
			case 'Min':
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
		}
	}
};
var $mdgriffith$elm_ui$Internal$Model$paddingNameFloat = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(top) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(right) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(bottom) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(left)))))));
	});
var $mdgriffith$elm_ui$Element$Input$redistributeOver = F4(
	function (isMultiline, stacked, attr, els) {
		switch (attr.$) {
			case 'Nearby':
				return _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					});
			case 'Width':
				var width = attr.a;
				return $mdgriffith$elm_ui$Element$Input$isFill(width) ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent),
						input: A2($elm$core$List$cons, attr, els.input),
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : (stacked ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent)
					}) : _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					}));
			case 'Height':
				var height = attr.a;
				return (!stacked) ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent),
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : ($mdgriffith$elm_ui$Element$Input$isFill(height) ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent),
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : ($mdgriffith$elm_ui$Element$Input$isPixel(height) ? _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					})));
			case 'AlignX':
				return _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent)
					});
			case 'AlignY':
				return _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent)
					});
			case 'StyleClass':
				switch (attr.b.$) {
					case 'SpacingStyle':
						var _v1 = attr.b;
						return _Utils_update(
							els,
							{
								fullParent: A2($elm$core$List$cons, attr, els.fullParent),
								input: A2($elm$core$List$cons, attr, els.input),
								parent: A2($elm$core$List$cons, attr, els.parent),
								wrapper: A2($elm$core$List$cons, attr, els.wrapper)
							});
					case 'PaddingStyle':
						var cls = attr.a;
						var _v2 = attr.b;
						var pad = _v2.a;
						var t = _v2.b;
						var r = _v2.c;
						var b = _v2.d;
						var l = _v2.e;
						if (isMultiline) {
							return _Utils_update(
								els,
								{
									cover: A2($elm$core$List$cons, attr, els.cover),
									parent: A2($elm$core$List$cons, attr, els.parent)
								});
						} else {
							var newTop = t - A2($elm$core$Basics$min, t, b);
							var newLineHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'line-height',
									'calc(1.0em + ' + ($elm$core$String$fromFloat(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'height',
									'calc(1.0em + ' + ($elm$core$String$fromFloat(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newBottom = b - A2($elm$core$Basics$min, t, b);
							var reducedVerticalPadding = A2(
								$mdgriffith$elm_ui$Internal$Model$StyleClass,
								$mdgriffith$elm_ui$Internal$Flag$padding,
								A5(
									$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
									A4($mdgriffith$elm_ui$Internal$Model$paddingNameFloat, newTop, r, newBottom, l),
									newTop,
									r,
									newBottom,
									l));
							return _Utils_update(
								els,
								{
									cover: A2($elm$core$List$cons, attr, els.cover),
									input: A2(
										$elm$core$List$cons,
										newHeight,
										A2($elm$core$List$cons, newLineHeight, els.input)),
									parent: A2($elm$core$List$cons, reducedVerticalPadding, els.parent)
								});
						}
					case 'BorderWidth':
						var _v3 = attr.b;
						return _Utils_update(
							els,
							{
								cover: A2($elm$core$List$cons, attr, els.cover),
								parent: A2($elm$core$List$cons, attr, els.parent)
							});
					case 'Transform':
						return _Utils_update(
							els,
							{
								cover: A2($elm$core$List$cons, attr, els.cover),
								parent: A2($elm$core$List$cons, attr, els.parent)
							});
					case 'FontSize':
						return _Utils_update(
							els,
							{
								fullParent: A2($elm$core$List$cons, attr, els.fullParent)
							});
					case 'FontFamily':
						var _v4 = attr.b;
						return _Utils_update(
							els,
							{
								fullParent: A2($elm$core$List$cons, attr, els.fullParent)
							});
					default:
						var flag = attr.a;
						var cls = attr.b;
						return _Utils_update(
							els,
							{
								parent: A2($elm$core$List$cons, attr, els.parent)
							});
				}
			case 'NoAttribute':
				return els;
			case 'Attr':
				var a = attr.a;
				return _Utils_update(
					els,
					{
						input: A2($elm$core$List$cons, attr, els.input)
					});
			case 'Describe':
				return _Utils_update(
					els,
					{
						input: A2($elm$core$List$cons, attr, els.input)
					});
			case 'Class':
				return _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					});
			default:
				return _Utils_update(
					els,
					{
						input: A2($elm$core$List$cons, attr, els.input)
					});
		}
	});
var $mdgriffith$elm_ui$Element$Input$redistribute = F3(
	function (isMultiline, stacked, attrs) {
		return function (redist) {
			return {
				cover: $elm$core$List$reverse(redist.cover),
				fullParent: $elm$core$List$reverse(redist.fullParent),
				input: $elm$core$List$reverse(redist.input),
				parent: $elm$core$List$reverse(redist.parent),
				wrapper: $elm$core$List$reverse(redist.wrapper)
			};
		}(
			A3(
				$elm$core$List$foldl,
				A2($mdgriffith$elm_ui$Element$Input$redistributeOver, isMultiline, stacked),
				{cover: _List_Nil, fullParent: _List_Nil, input: _List_Nil, parent: _List_Nil, wrapper: _List_Nil},
				attrs));
	});
var $mdgriffith$elm_ui$Element$Input$renderBox = function (_v0) {
	var top = _v0.top;
	var right = _v0.right;
	var bottom = _v0.bottom;
	var left = _v0.left;
	return $elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px'))))));
};
var $mdgriffith$elm_ui$Element$alpha = function (o) {
	var transparency = function (x) {
		return 1 - x;
	}(
		A2(
			$elm$core$Basics$min,
			1.0,
			A2($elm$core$Basics$max, 0.0, o)));
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Transparency,
			'transparency-' + $mdgriffith$elm_ui$Internal$Model$floatClass(transparency),
			transparency));
};
var $mdgriffith$elm_ui$Element$Input$charcoal = A3($mdgriffith$elm_ui$Element$rgb, 136 / 255, 138 / 255, 133 / 255);
var $mdgriffith$elm_ui$Element$Input$renderPlaceholder = F3(
	function (_v0, forPlaceholder, on) {
		var placeholderAttrs = _v0.a;
		var placeholderEl = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_Utils_ap(
				forPlaceholder,
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$color($mdgriffith$elm_ui$Element$Input$charcoal),
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.noTextSelection + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.passPointerEvents)),
							$mdgriffith$elm_ui$Element$clip,
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$Background$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$alpha(
							on ? 1 : 0)
						]),
					placeholderAttrs)),
			placeholderEl);
	});
var $mdgriffith$elm_ui$Element$scrollbarY = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.scrollbarsY);
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $mdgriffith$elm_ui$Element$Input$spellcheck = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$spellcheck);
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $mdgriffith$elm_ui$Element$Input$value = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$value);
var $mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var withDefaults = _Utils_ap($mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var redistributed = A3(
			$mdgriffith$elm_ui$Element$Input$redistribute,
			_Utils_eq(textInput.type_, $mdgriffith$elm_ui$Element$Input$TextArea),
			$mdgriffith$elm_ui$Element$Input$isStacked(textOptions.label),
			withDefaults);
		var onlySpacing = function (attr) {
			if ((attr.$ === 'StyleClass') && (attr.b.$ === 'SpacingStyle')) {
				var _v9 = attr.b;
				return true;
			} else {
				return false;
			}
		};
		var heightConstrained = function () {
			var _v7 = textInput.type_;
			if (_v7.$ === 'TextInputNode') {
				var inputType = _v7.a;
				return false;
			} else {
				return A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$mdgriffith$elm_ui$Element$Input$isConstrained,
						$elm$core$List$head(
							$elm$core$List$reverse(
								A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Element$Input$getHeight, withDefaults)))));
			}
		}();
		var getPadding = function (attr) {
			if ((attr.$ === 'StyleClass') && (attr.b.$ === 'PaddingStyle')) {
				var cls = attr.a;
				var _v6 = attr.b;
				var pad = _v6.a;
				var t = _v6.b;
				var r = _v6.c;
				var b = _v6.d;
				var l = _v6.e;
				return $elm$core$Maybe$Just(
					{
						bottom: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(b - 3)),
						left: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(l - 3)),
						right: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(r - 3)),
						top: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(t - 3))
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var parentPadding = A2(
			$elm$core$Maybe$withDefault,
			{bottom: 0, left: 0, right: 0, top: 0},
			$elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$List$filterMap, getPadding, withDefaults))));
		var inputElement = A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			function () {
				var _v3 = textInput.type_;
				if (_v3.$ === 'TextInputNode') {
					var inputType = _v3.a;
					return $mdgriffith$elm_ui$Internal$Model$NodeName('input');
				} else {
					return $mdgriffith$elm_ui$Internal$Model$NodeName('textarea');
				}
			}(),
			_Utils_ap(
				function () {
					var _v4 = textInput.type_;
					if (_v4.$ === 'TextInputNode') {
						var inputType = _v4.a;
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Internal$Model$Attr(
								$elm$html$Html$Attributes$type_(inputType)),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputText)
							]);
					} else {
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Element$clip,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputMultiline),
								$mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding(withDefaults),
								$mdgriffith$elm_ui$Element$paddingEach(parentPadding),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2(
									$elm$html$Html$Attributes$style,
									'margin',
									$mdgriffith$elm_ui$Element$Input$renderBox(
										$mdgriffith$elm_ui$Element$Input$negateBox(parentPadding)))),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2($elm$html$Html$Attributes$style, 'box-sizing', 'content-box'))
							]);
					}
				}(),
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Input$value(textOptions.text),
							$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Events$onInput(textOptions.onChange)),
							$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions.label),
							$mdgriffith$elm_ui$Element$Input$spellcheck(textInput.spellchecked),
							A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Internal$Model$NoAttribute,
							A2($elm$core$Maybe$map, $mdgriffith$elm_ui$Element$Input$autofill, textInput.autofill))
						]),
					redistributed.input)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil));
		var wrappedInput = function () {
			var _v0 = textInput.type_;
			if (_v0.$ === 'TextArea') {
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					_Utils_ap(
						(heightConstrained ? $elm$core$List$cons($mdgriffith$elm_ui$Element$scrollbarY) : $elm$core$Basics$identity)(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.focusedWithin),
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineWrapper)
								])),
						redistributed.parent),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								$mdgriffith$elm_ui$Internal$Model$element,
								$mdgriffith$elm_ui$Internal$Model$asParagraph,
								$mdgriffith$elm_ui$Internal$Model$div,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Element$inFront(inputElement),
											A2(
												$elm$core$List$cons,
												$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineParent),
												redistributed.wrapper)))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(
									function () {
										if (textOptions.text === '') {
											var _v1 = textOptions.placeholder;
											if (_v1.$ === 'Nothing') {
												return _List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('\u00A0')
													]);
											} else {
												var place = _v1.a;
												return _List_fromArray(
													[
														A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, _List_Nil, textOptions.text === '')
													]);
											}
										} else {
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Internal$Model$unstyled(
													A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineFiller)
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(textOptions.text + '\u00A0')
															])))
												]);
										}
									}()))
							])));
			} else {
				var inputType = _v0.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						A2(
							$elm$core$List$cons,
							A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.focusedWithin),
							$elm$core$List$concat(
								_List_fromArray(
									[
										redistributed.parent,
										function () {
										var _v2 = textOptions.placeholder;
										if (_v2.$ === 'Nothing') {
											return _List_Nil;
										} else {
											var place = _v2.a;
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$behindContent(
													A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, redistributed.cover, textOptions.text === ''))
												]);
										}
									}()
									])))),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[inputElement])));
			}
		}();
		return A3(
			$mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				$elm$core$List$cons,
				A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cursorText),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions.label) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(5),
					A2($elm$core$List$cons, $mdgriffith$elm_ui$Element$Region$announce, redistributed.fullParent))),
			textOptions.label,
			wrappedInput);
	});
var $mdgriffith$elm_ui$Element$Input$multiline = F2(
	function (attrs, multi) {
		return A3(
			$mdgriffith$elm_ui$Element$Input$textHelper,
			{autofill: $elm$core$Maybe$Nothing, spellchecked: multi.spellcheck, type_: $mdgriffith$elm_ui$Element$Input$TextArea},
			attrs,
			{label: multi.label, onChange: multi.onChange, placeholder: multi.placeholder, text: multi.text});
	});
var $elm$html$Html$Events$onBlur = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'blur',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 'TextInputNode', a: a};
};
var $mdgriffith$elm_ui$Element$Input$text = $mdgriffith$elm_ui$Element$Input$textHelper(
	{
		autofill: $elm$core$Maybe$Nothing,
		spellchecked: false,
		type_: $mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var $author$project$Views$Chat$userView = function (_v0) {
	var userId = _v0.a;
	var username = _v0.b;
	return $author$project$Common$Contents$plainPara(
		username + (' (' + ($elm$core$String$fromInt(userId) + ')')));
};
var $author$project$Views$Chat$chatView = function (model) {
	return A2(
		$mdgriffith$elm_ui$Element$row,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
				A2($mdgriffith$elm_ui$Element$spacingXY, $author$project$Views$Chat$sideColumnGap, 0)
			]),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width(
						A2(
							$mdgriffith$elm_ui$Element$maximum,
							$author$project$Views$Chat$chatColumnMaxWidthPx(model.viewport.viewport.width),
							$mdgriffith$elm_ui$Element$fill)),
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
					]),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Element$paddingEach(
								{bottom: 0, left: 0, right: 30, top: 0}),
								A2($mdgriffith$elm_ui$Element$spacingXY, 0, 30),
								$mdgriffith$elm_ui$Element$scrollbarY,
								$mdgriffith$elm_ui$Element$htmlAttribute(
								$elm$html$Html$Attributes$id($author$project$Views$Chat$msgsViewHtmlId)),
								$mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Events$on,
									'scroll',
									$elm$json$Json$Decode$succeed(
										$author$project$CoreTypes$ChatMsgsViewEvent($author$project$Chat$OnManualScrolled))))
							]),
						A2(
							$elm$core$List$map,
							$author$project$Views$Chat$msgBundleView,
							$author$project$Chat$mkMsgBundles(model.chatStatus.msgs))),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$height(
								$mdgriffith$elm_ui$Element$px(40)),
								$mdgriffith$elm_ui$Element$centerX
							]),
						function () {
							var _v0 = model.chatStatus.shouldHintNewMsg;
							if (!_v0) {
								return $mdgriffith$elm_ui$Element$none;
							} else {
								return A2(
									$mdgriffith$elm_ui$Element$Input$button,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$centerY,
											$mdgriffith$elm_ui$Element$padding(5),
											$mdgriffith$elm_ui$Element$Border$width(2),
											$mdgriffith$elm_ui$Element$Border$rounded(6)
										]),
									{
										label: $mdgriffith$elm_ui$Element$text('New Messages Received'),
										onPress: $elm$core$Maybe$Just(
											$author$project$CoreTypes$ChatMsgsViewEvent($author$project$Chat$OnNewMsgHintClicked))
									});
							}
						}()),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
							]),
						A2(
							$mdgriffith$elm_ui$Element$Input$multiline,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$height(
									$mdgriffith$elm_ui$Element$px(200)),
									$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor),
									$mdgriffith$elm_ui$Element$htmlAttribute(
									$elm$html$Html$Events$onFocus(
										$author$project$CoreTypes$OnChatInputFocal(true))),
									$mdgriffith$elm_ui$Element$htmlAttribute(
									$elm$html$Html$Events$onBlur(
										$author$project$CoreTypes$OnChatInputFocal(false)))
								]),
							{
								label: A2($mdgriffith$elm_ui$Element$Input$labelAbove, _List_Nil, $mdgriffith$elm_ui$Element$none),
								onChange: $author$project$CoreTypes$MessageInput,
								placeholder: $elm$core$Maybe$Nothing,
								spellcheck: false,
								text: $joneshf$elm_tagged$Tagged$untag(model.chatStatus.input)
							})),
						A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Element$paddingXY, 0, 10),
								A2($mdgriffith$elm_ui$Element$spacingXY, 10, 0)
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$Input$button,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$padding(5),
										$mdgriffith$elm_ui$Element$Background$color(
										A3($mdgriffith$elm_ui$Element$rgb255, 0, 100, 0))
									]),
								{
									label: $mdgriffith$elm_ui$Element$text('Send'),
									onPress: $elm$core$Maybe$Just($author$project$CoreTypes$MessageSend)
								}),
								A2(
								$mdgriffith$elm_ui$Element$Input$text,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor)
									]),
								{
									label: A2(
										$mdgriffith$elm_ui$Element$Input$labelLeft,
										_List_Nil,
										A2(
											$mdgriffith$elm_ui$Element$Input$button,
											_List_Nil,
											{
												label: $mdgriffith$elm_ui$Element$text('Change Name: '),
												onPress: $elm$core$Maybe$Just($author$project$CoreTypes$NameChange)
											})),
									onChange: $author$project$CoreTypes$NewNameInput,
									placeholder: $elm$core$Maybe$Nothing,
									text: model.newNameInput
								})
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width(
						$mdgriffith$elm_ui$Element$px($author$project$Views$Chat$sideColumnWidthPx)),
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
						A2($mdgriffith$elm_ui$Element$paddingXY, 30, 20),
						$mdgriffith$elm_ui$Element$Border$widthEach(
						{bottom: 0, left: 2, right: 0, top: 0})
					]),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Element$paragraph,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$Font$size(24)
							]),
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$text('Users')
							])),
						A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Element$height(
								$mdgriffith$elm_ui$Element$fillPortion(4)),
								A2($mdgriffith$elm_ui$Element$paddingXY, 0, 40),
								A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20),
								$mdgriffith$elm_ui$Element$scrollbarY
							]),
						A2(
							$elm$core$List$map,
							$author$project$Views$Chat$userView,
							$elm$core$Dict$toList(model.chatStatus.users))),
						A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$height(
								$mdgriffith$elm_ui$Element$fillPortion(1)),
								$mdgriffith$elm_ui$Element$paddingEach(
								{bottom: 0, left: 0, right: 0, top: 20}),
								A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10),
								$mdgriffith$elm_ui$Element$Border$widthEach(
								{bottom: 0, left: 0, right: 0, top: 2})
							]),
						_List_fromArray(
							[
								$author$project$Common$Contents$plainPara(
								'Current Users: ' + $elm$core$String$fromInt(
									$elm$core$Dict$size(model.chatStatus.users))),
								$author$project$Common$Contents$plainPara(
								'Join Count: ' + $elm$core$String$fromInt(model.chatStatus.joinCount)),
								$author$project$Common$Contents$plainPara(
								'Max Join Count: ' + function () {
									var _v1 = model.chatStatus.maxJoinCount;
									if (_v1.$ === 'Nothing') {
										return 'Unlimited';
									} else {
										var n = _v1.a;
										return $elm$core$String$fromInt(n);
									}
								}())
							]))
					]))
			]));
};
var $author$project$Common$Styles$linkStyle = _List_fromArray(
	[$mdgriffith$elm_ui$Element$Font$underline]);
var $author$project$Common$Contents$link = F2(
	function (urlStr, labelStr) {
		return A2(
			$mdgriffith$elm_ui$Element$link,
			$author$project$Common$Styles$linkStyle,
			{
				label: $mdgriffith$elm_ui$Element$text(labelStr),
				url: urlStr
			});
	});
var $author$project$Common$Urls$rootUrl = A2($elm$url$Url$Builder$absolute, _List_Nil, _List_Nil);
var $author$project$Common$Styles$widthConstraint = $mdgriffith$elm_ui$Element$width(
	A2($mdgriffith$elm_ui$Element$maximum, 750, $mdgriffith$elm_ui$Element$fill));
var $author$project$Common$Contents$footer = A2(
	$mdgriffith$elm_ui$Element$row,
	_List_fromArray(
		[
			$author$project$Common$Styles$widthConstraint,
			A2($mdgriffith$elm_ui$Element$paddingXY, 0, 20),
			$mdgriffith$elm_ui$Element$Border$widthEach(
			{bottom: 0, left: 0, right: 0, top: 2})
		]),
	_List_fromArray(
		[
			A2($author$project$Common$Contents$link, $author$project$Common$Urls$rootUrl, 'Hideout Home')
		]));
var $author$project$Common$Contents$newTabLink = F2(
	function (urlStr, labelStr) {
		return A2(
			$mdgriffith$elm_ui$Element$newTabLink,
			$author$project$Common$Styles$linkStyle,
			{
				label: $mdgriffith$elm_ui$Element$text(labelStr),
				url: urlStr
			});
	});
var $author$project$Views$Chat$view = function (model) {
	var _v0 = model.chatStatus.err;
	if (_v0.$ === 'Nothing') {
		return $author$project$Views$Chat$chatView(model);
	} else {
		var err = _v0.a;
		var errContent = function () {
			if (err.$ === 'MaxJoined') {
				return A2(
					$mdgriffith$elm_ui$Element$paragraph,
					_List_Nil,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$text('\n                                Hi, welcome to Hideout! Unfortunately, this chat room has reached the maximum number of times it can be joined. Read more about what this implies \n                                '),
							A2($author$project$Common$Contents$newTabLink, $author$project$Common$Urls$aboutUrl, 'here'),
							$mdgriffith$elm_ui$Element$text('.')
						]));
			} else {
				return A2(
					$mdgriffith$elm_ui$Element$column,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Common$Contents$plainPara('\n                                Hi, welcome to Hideout! This chat room doesn\'t exist. The reason can be:\n                                '),
							A2(
							$mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Element$paddingXY, 0, 20),
									A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10)
								]),
							_List_fromArray(
								[
									$author$project$Common$Contents$plainPara('- Either you entered a wrong link or chat ID;'),
									$author$project$Common$Contents$plainPara('- Or the chat is expired and deleted.')
								]))
						]));
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					A2($mdgriffith$elm_ui$Element$maximum, 750, $mdgriffith$elm_ui$Element$fill)),
					A2($mdgriffith$elm_ui$Element$spacingXY, 0, 100)
				]),
			_List_fromArray(
				[errContent, $author$project$Common$Contents$footer]));
	}
};
var $author$project$CoreTypes$DispChatMaxJoinCountInput = function (a) {
	return {$: 'DispChatMaxJoinCountInput', a: a};
};
var $author$project$CoreTypes$PersistChatMaxJoinCountInput = function (a) {
	return {$: 'PersistChatMaxJoinCountInput', a: a};
};
var $author$project$CoreTypes$SpawnDispChat = {$: 'SpawnDispChat'};
var $author$project$CoreTypes$SpawnPersistChat = {$: 'SpawnPersistChat'};
var $author$project$Common$Contents$borderedButton = F2(
	function (msg, labelStr) {
		return A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$padding(5),
					$mdgriffith$elm_ui$Element$Border$width(2),
					$mdgriffith$elm_ui$Element$Border$rounded(6)
				]),
			{
				label: $mdgriffith$elm_ui$Element$text(labelStr),
				onPress: $elm$core$Maybe$Just(msg)
			});
	});
var $author$project$Common$Urls$frontendReadLetterUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['read-letter']),
	_List_Nil);
var $author$project$Common$Contents$italicText = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[$mdgriffith$elm_ui$Element$Font$italic]),
		$mdgriffith$elm_ui$Element$text(str));
};
var $mdgriffith$elm_ui$Element$Input$HiddenLabel = function (a) {
	return {$: 'HiddenLabel', a: a};
};
var $mdgriffith$elm_ui$Element$Input$labelHidden = $mdgriffith$elm_ui$Element$Input$HiddenLabel;
var $author$project$Views$ConfigChat$numParticipantsInput = F2(
	function (msg, input) {
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Element$paddingXY, 0, 20)
				]),
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$text('Number of participants: '),
					A2(
					$mdgriffith$elm_ui$Element$Input$text,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width(
							$mdgriffith$elm_ui$Element$px(100)),
							$mdgriffith$elm_ui$Element$height(
							A2($mdgriffith$elm_ui$Element$maximum, 40, $mdgriffith$elm_ui$Element$fill)),
							$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor)
						]),
					{
						label: $mdgriffith$elm_ui$Element$Input$labelHidden(''),
						onChange: msg,
						placeholder: $elm$core$Maybe$Nothing,
						text: input
					})
				]));
	});
var $author$project$Common$Contents$posIntInputHint = function (input) {
	var _v0 = $author$project$Utils$Types$strToPosIntInput(input);
	if (_v0.$ === 'Good') {
		return $mdgriffith$elm_ui$Element$none;
	} else {
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$red)
				]),
			$mdgriffith$elm_ui$Element$text('Please input a positive integer.'));
	}
};
var $elm$url$Url$Builder$relative = F2(
	function (pathSegments, parameters) {
		return _Utils_ap(
			A2($elm$core$String$join, '/', pathSegments),
			$elm$url$Url$Builder$toQuery(parameters));
	});
var $author$project$Common$Contents$underlinedText = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[$mdgriffith$elm_ui$Element$Font$underline]),
		$mdgriffith$elm_ui$Element$text(str));
};
var $author$project$Views$ConfigChat$view = function (model) {
	return A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Common$Styles$widthConstraint]),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_Nil,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('A '),
						$author$project$Common$Contents$italicText('disposable'),
						$mdgriffith$elm_ui$Element$text('\n                 chat is a good default option. Only you and your friends can join. The server deletes the chat when the room is empty.\n                ')
					])),
				A2($author$project$Views$ConfigChat$numParticipantsInput, $author$project$CoreTypes$DispChatMaxJoinCountInput, model.dispChatMaxJoinCountInput),
				$author$project$Common$Contents$posIntInputHint(model.dispChatMaxJoinCountInput),
				A2(
				$mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$paddingEach(
						{bottom: 0, left: 0, right: 0, top: 10})
					]),
				A2($author$project$Common$Contents$borderedButton, $author$project$CoreTypes$SpawnDispChat, 'Start a disposable chat!')),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$paddingEach(
						{bottom: 0, left: 0, right: 0, top: 200})
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('A '),
						$author$project$Common$Contents$italicText('persistent'),
						$mdgriffith$elm_ui$Element$text('\n                 chat is more convenient. You can bookmark the chat, and send private messages to your contacts without having to make a new chat every time. This especially helps if you need to circumvent censorship. Sharing too many Hideout links may draw unwanted attention.\n                ')
					])),
				A2($author$project$Views$ConfigChat$numParticipantsInput, $author$project$CoreTypes$PersistChatMaxJoinCountInput, model.persistChatMaxJoinCountInput),
				$author$project$Common$Contents$posIntInputHint(model.persistChatMaxJoinCountInput),
				function () {
				var _v0 = model.userStatus;
				if (_v0.$ === 'GotPersistChatIdLetter') {
					var result = _v0.a;
					if (result.$ === 'Err') {
						return $mdgriffith$elm_ui$Element$text('Error!');
					} else {
						var letterId = result.a;
						return A2(
							$mdgriffith$elm_ui$Element$textColumn,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$paddingEach(
									{bottom: 0, left: 0, right: 0, top: 20}),
									A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20)
								]),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Element$paragraph,
									_List_Nil,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$text('\n                                A disposable letter that contains the instruction to join the chat has been generated. Share the \n                                '),
											$author$project$Common$Contents$underlinedText('link'),
											$mdgriffith$elm_ui$Element$text('\n                                 (not the content) to the letter below with your contacts.\n                                ')
										])),
									$author$project$Common$Contents$plainPara(
									A2(
										$elm$url$Url$Builder$relative,
										_List_fromArray(
											[
												$author$project$Common$Urls$frontendReadLetterUrl,
												$elm_community$string_extra$String$Extra$unquote(letterId)
											]),
										_List_Nil))
								]));
					}
				} else {
					return A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$paddingEach(
								{bottom: 0, left: 0, right: 0, top: 10})
							]),
						A2($author$project$Common$Contents$borderedButton, $author$project$CoreTypes$SpawnPersistChat, 'Start a persistent chat!'));
				}
			}()
			]));
};
var $author$project$Utils$Errors$httpErrToStr = function (err) {
	switch (err.$) {
		case 'BadUrl':
			var str = err.a;
			return 'Bad URL ' + str;
		case 'Timeout':
			return 'Timeout';
		case 'NetworkError':
			return '\n            Can\'t reach server. Either your network has a problem, or server is down.\n            ';
		case 'BadStatus':
			var code = err.a;
			return '\n            Bad status code: \n            ' + $elm$core$String$fromInt(code);
		default:
			var str = err.a;
			return '\n            Bad response body: \n            ' + str;
	}
};
var $author$project$Utils$Utils$intToOrdStr = function (n) {
	var postfix = ((A2($elm$core$Basics$modBy, 100, n) >= 10) && (A2($elm$core$Basics$modBy, 100, n) <= 20)) ? 'th' : ((A2($elm$core$Basics$modBy, 10, n) === 1) ? 'st' : ((A2($elm$core$Basics$modBy, 10, n) === 2) ? 'nd' : ((A2($elm$core$Basics$modBy, 10, n) === 3) ? 'rd' : 'th')));
	return _Utils_ap(
		$elm$core$String$fromInt(n),
		postfix);
};
var $author$project$Views$ReadLetter$view = function (model) {
	var windowWidth = model.viewport.viewport.width;
	var maxWidthPx = $elm$core$Basics$round(
		windowWidth - (2 * $author$project$Common$Styles$windowPaddingPx(windowWidth)));
	var intro = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width(
				A2($mdgriffith$elm_ui$Element$maximum, 750, $mdgriffith$elm_ui$Element$fill))
			]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('You received a Hideout letter!'),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$paddingEach(
						{bottom: 0, left: 0, right: 0, top: 20})
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('\n                        Hideout is a service for private messaging. This letter can be read a strictly limited times. So please don\'t refresh or reopen this letter. It prevents other recipients from accessing it!\n                        ')
					]))
			]));
	return A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width(
				A2($mdgriffith$elm_ui$Element$maximum, maxWidthPx, $mdgriffith$elm_ui$Element$fill))
			]),
		_List_fromArray(
			[
				function () {
				var _v0 = model.letterStatus.read;
				switch (_v0.$) {
					case 'Waiting':
						return $author$project$Common$Contents$plainPara('Waiting for the letter from server..');
					case 'Got':
						var result = _v0.a;
						if (result.$ === 'Err') {
							var err = result.a;
							if ((err.$ === 'BadStatus') && (err.a === 404)) {
								return A2(
									$mdgriffith$elm_ui$Element$column,
									_List_fromArray(
										[$author$project$Common$Styles$widthConstraint]),
									_List_fromArray(
										[
											$author$project$Common$Contents$plainPara('\n                                        Hi, welcome to Hideout! The letter can\'t be found. The reason is:\n                                        '),
											A2(
											$mdgriffith$elm_ui$Element$column,
											_List_fromArray(
												[
													$mdgriffith$elm_ui$Element$paddingEach(
													{bottom: 100, left: 0, right: 0, top: 20}),
													A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10)
												]),
											_List_fromArray(
												[
													$author$project$Common$Contents$plainPara('- Either you entered a wrong link or letter ID;'),
													A2(
													$mdgriffith$elm_ui$Element$paragraph,
													_List_Nil,
													_List_fromArray(
														[
															$mdgriffith$elm_ui$Element$text('\n                                                - Or the letter has reached the maximum number of times it can be read. Read more about what it implies \n                                                '),
															A2($author$project$Common$Contents$newTabLink, $author$project$Common$Urls$aboutUrl, 'here'),
															$mdgriffith$elm_ui$Element$text('.')
														]))
												])),
											$author$project$Common$Contents$footer
										]));
							} else {
								return $author$project$Common$Contents$plainPara(
									$author$project$Utils$Errors$httpErrToStr(err));
							}
						} else {
							var letterMeta = result.a;
							return A2(
								$mdgriffith$elm_ui$Element$column,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								_List_fromArray(
									[
										intro,
										A2(
										$mdgriffith$elm_ui$Element$textColumn,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Element$paddingXY, 0, 40),
												A2($mdgriffith$elm_ui$Element$spacingXY, 0, 5)
											]),
										_List_fromArray(
											[
												A2(
												$mdgriffith$elm_ui$Element$paragraph,
												_List_Nil,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('This letter is being read for the '),
														$mdgriffith$elm_ui$Element$text(
														$author$project$Utils$Utils$intToOrdStr(letterMeta.readCount)),
														$mdgriffith$elm_ui$Element$text(' time.')
													])),
												A2(
												$mdgriffith$elm_ui$Element$paragraph,
												_List_Nil,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('It can be read at most '),
														$mdgriffith$elm_ui$Element$text(
														$elm$core$String$fromInt(letterMeta.letter.maxReadCount)),
														$mdgriffith$elm_ui$Element$text(' times.')
													]))
											])),
										A2(
										$mdgriffith$elm_ui$Element$paragraph,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$paddingEach(
												{bottom: 20, left: 0, right: 0, top: 0})
											]),
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$text('Below is the letter.')
											])),
										A2(
										$mdgriffith$elm_ui$Element$column,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$paddingEach(
												{bottom: 0, left: 0, right: 0, top: 60}),
												$mdgriffith$elm_ui$Element$Border$widthEach(
												{bottom: 0, left: 0, right: 0, top: 2})
											]),
										$author$project$Utils$Markdown$render(letterMeta.letter.body))
									]));
						}
					default:
						return $author$project$Common$Contents$plainPara('Error: Unaddressed UserStatus case!');
				}
			}()
			]));
};
var $author$project$CoreTypes$LetterInput = function (a) {
	return {$: 'LetterInput', a: a};
};
var $author$project$CoreTypes$LetterMaxReadCountInput = function (a) {
	return {$: 'LetterMaxReadCountInput', a: a};
};
var $author$project$CoreTypes$LetterPersistInput = function (a) {
	return {$: 'LetterPersistInput', a: a};
};
var $author$project$CoreTypes$LetterSend = {$: 'LetterSend'};
var $author$project$Common$Styles$buttonStyle = function (padding) {
	return _List_fromArray(
		[
			$mdgriffith$elm_ui$Element$padding(padding),
			$mdgriffith$elm_ui$Element$Border$width(2),
			$mdgriffith$elm_ui$Element$Border$rounded(6)
		]);
};
var $mdgriffith$elm_ui$Internal$Model$Left = {$: 'Left'};
var $mdgriffith$elm_ui$Element$alignLeft = $mdgriffith$elm_ui$Internal$Model$AlignX($mdgriffith$elm_ui$Internal$Model$Left);
var $mdgriffith$elm_ui$Element$Input$tabindex = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$tabindex);
var $mdgriffith$elm_ui$Element$Input$checkbox = F2(
	function (attrs, _v0) {
		var label = _v0.label;
		var icon = _v0.icon;
		var checked = _v0.checked;
		var onChange = _v0.onChange;
		var attributes = _Utils_ap(
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(label) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(6),
					$mdgriffith$elm_ui$Internal$Model$Attr(
					$elm$html$Html$Events$onClick(
						onChange(!checked))),
					$mdgriffith$elm_ui$Element$Region$announce,
					$mdgriffith$elm_ui$Element$Input$onKeyLookup(
					function (code) {
						return _Utils_eq(code, $mdgriffith$elm_ui$Element$Input$enter) ? $elm$core$Maybe$Just(
							onChange(!checked)) : (_Utils_eq(code, $mdgriffith$elm_ui$Element$Input$space) ? $elm$core$Maybe$Just(
							onChange(!checked)) : $elm$core$Maybe$Nothing);
					}),
					$mdgriffith$elm_ui$Element$Input$tabindex(0),
					$mdgriffith$elm_ui$Element$pointer,
					$mdgriffith$elm_ui$Element$alignLeft,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
				]),
			attrs);
		return A3(
			$mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Attr(
					A2($elm$html$Html$Attributes$attribute, 'role', 'checkbox')),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Internal$Model$Attr(
						A2(
							$elm$html$Html$Attributes$attribute,
							'aria-checked',
							checked ? 'true' : 'false')),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(label),
						attributes))),
			label,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$centerY,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink)
					]),
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[
							icon(checked)
						]))));
	});
var $author$project$Views$WriteLetter$dividerWidth = 100;
var $author$project$Views$WriteLetter$divider = A2(
	$mdgriffith$elm_ui$Element$el,
	_List_fromArray(
		[
			$mdgriffith$elm_ui$Element$width(
			$mdgriffith$elm_ui$Element$px($author$project$Views$WriteLetter$dividerWidth))
		]),
	$mdgriffith$elm_ui$Element$none);
var $mdgriffith$elm_ui$Element$Input$OnRight = {$: 'OnRight'};
var $mdgriffith$elm_ui$Element$Input$labelRight = $mdgriffith$elm_ui$Element$Input$Label($mdgriffith$elm_ui$Element$Input$OnRight);
var $author$project$Common$Styles$lineSpacing = A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10);
var $author$project$Views$WriteLetter$view = function (model) {
	var windowWidth = model.viewport.viewport.width;
	var persistInput = A2(
		$mdgriffith$elm_ui$Element$Input$checkbox,
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Element$paddingXY, 0, 20)
			]),
		{
			checked: model.letterPersistInput,
			icon: $mdgriffith$elm_ui$Element$Input$defaultCheckbox,
			label: A2(
				$mdgriffith$elm_ui$Element$Input$labelRight,
				_List_Nil,
				$mdgriffith$elm_ui$Element$text('Disk Persistence')),
			onChange: $author$project$CoreTypes$LetterPersistInput
		});
	var maxReadCountInput = A2(
		$mdgriffith$elm_ui$Element$row,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$paddingEach(
				{bottom: 10, left: 0, right: 0, top: 40}),
				A2($mdgriffith$elm_ui$Element$spacingXY, 10, 0)
			]),
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$text('This letter can be read '),
				A2(
				$mdgriffith$elm_ui$Element$Input$text,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width(
						$mdgriffith$elm_ui$Element$px(100)),
						$mdgriffith$elm_ui$Element$height(
						A2($mdgriffith$elm_ui$Element$maximum, 40, $mdgriffith$elm_ui$Element$fill)),
						$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor)
					]),
				{
					label: $mdgriffith$elm_ui$Element$Input$labelHidden(''),
					onChange: $author$project$CoreTypes$LetterMaxReadCountInput,
					placeholder: $elm$core$Maybe$Nothing,
					text: model.letterRawInput.maxReadCount
				}),
				$mdgriffith$elm_ui$Element$text(' times.')
			]));
	var letterInputWidthConstraint = $mdgriffith$elm_ui$Element$width(
		A2(
			$mdgriffith$elm_ui$Element$maximum,
			$elm$core$Basics$round(
				((windowWidth - (2 * $author$project$Common$Styles$windowPaddingPx(windowWidth))) - $author$project$Views$WriteLetter$dividerWidth) / 2),
			$mdgriffith$elm_ui$Element$fill));
	var preview = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				letterInputWidthConstraint,
				$mdgriffith$elm_ui$Element$spacing(30),
				$mdgriffith$elm_ui$Element$alignTop
			]),
		$author$project$Utils$Markdown$render(model.letterRawInput.body));
	var letterInputBox = A2(
		$mdgriffith$elm_ui$Element$Input$multiline,
		_List_fromArray(
			[
				letterInputWidthConstraint,
				$mdgriffith$elm_ui$Element$alignTop,
				$mdgriffith$elm_ui$Element$scrollbarY,
				$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor)
			]),
		{
			label: A2($mdgriffith$elm_ui$Element$Input$labelAbove, _List_Nil, $mdgriffith$elm_ui$Element$none),
			onChange: $author$project$CoreTypes$LetterInput,
			placeholder: $elm$core$Maybe$Nothing,
			spellcheck: false,
			text: model.letterRawInput.body
		});
	var instruction = A2(
		$mdgriffith$elm_ui$Element$textColumn,
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20)
			]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('Type away your letter below. Markdown is supported.'),
				$author$project$Common$Contents$plainPara('\n                    Send the letter after it\'s finished. It will be saved to a link, that can be accessed a strictly limited number of times. So don\'t click that link yourself. Just give it to your contacts.\n                    '),
				$author$project$Common$Contents$plainPara('\n                    The server deletes the letter after the access limit is reached. So if all of your contacts report that they\'ve read the letter, you\'ll be sure that nobody else could have read it.\n                    ')
			]));
	return A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
			]),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[$author$project$Common$Styles$widthConstraint]),
				_List_fromArray(
					[
						instruction,
						maxReadCountInput,
						$author$project$Common$Contents$posIntInputHint(model.letterRawInput.maxReadCount),
						persistInput
					])),
				A2(
				$mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
					]),
				_List_fromArray(
					[
						function () {
						var _v0 = model.letterStatus.write;
						switch (_v0.$) {
							case 'NotSent':
								return letterInputBox;
							case 'Sent':
								return A2(
									$mdgriffith$elm_ui$Element$paragraph,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
										]),
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$text('Letter is sent. Waiting for the letter ID from server...')
										]));
							default:
								var result = _v0.a;
								if (result.$ === 'Err') {
									var err = result.a;
									return $author$project$Common$Contents$plainPara(
										$author$project$Utils$Errors$httpErrToStr(err));
								} else {
									var info = result.a;
									return A2(
										$mdgriffith$elm_ui$Element$textColumn,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$author$project$Common$Styles$lineSpacing,
												$mdgriffith$elm_ui$Element$padding(10),
												$mdgriffith$elm_ui$Element$Border$width(2),
												$mdgriffith$elm_ui$Element$Border$rounded(6)
											]),
										_List_fromArray(
											[
												A2(
												$mdgriffith$elm_ui$Element$paragraph,
												_List_Nil,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('Your letter can be read '),
														$mdgriffith$elm_ui$Element$text(
														$elm$core$String$fromInt(info.maxReadCount)),
														$mdgriffith$elm_ui$Element$text(' times, at:')
													])),
												$author$project$Common$Contents$plainPara(
												$author$project$Common$Urls$frontendReadLetterUrl + ('/' + $elm_community$string_extra$String$Extra$unquote(info.id)))
											]));
								}
						}
					}(),
						$author$project$Views$WriteLetter$divider,
						preview
					])),
				function () {
				var _v2 = model.letterStatus.write;
				if (_v2.$ === 'NotSent') {
					return A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$paddingEach(
								{bottom: 0, left: 0, right: 0, top: 20})
							]),
						A2(
							$mdgriffith$elm_ui$Element$Input$button,
							$author$project$Common$Styles$buttonStyle(5),
							{
								label: $mdgriffith$elm_ui$Element$text('Send'),
								onPress: $elm$core$Maybe$Just($author$project$CoreTypes$LetterSend)
							}));
				} else {
					return $mdgriffith$elm_ui$Element$none;
				}
			}()
			]));
};
var $author$project$Common$Colors$white = A3($mdgriffith$elm_ui$Element$rgb255, 255, 255, 255);
var $author$project$Common$Styles$windowPadding = function (windowWidth) {
	return $mdgriffith$elm_ui$Element$padding(
		$elm$core$Basics$round(
			$author$project$Common$Styles$windowPaddingPx(windowWidth)));
};
var $author$project$Main$view = function (model) {
	return {
		body: _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$layout,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor),
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$white)
					]),
				A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$author$project$Common$Styles$windowPadding(model.viewport.viewport.width)
						]),
					function () {
						var _v0 = model.route;
						switch (_v0.$) {
							case 'Root':
								return A2(
									$mdgriffith$elm_ui$Element$textColumn,
									_List_Nil,
									_List_fromArray(
										[
											$author$project$Common$Contents$plainPara('Welcome to Hideout - A Service for Disposable Messages!'),
											A2(
											$mdgriffith$elm_ui$Element$textColumn,
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Element$paddingXY, 0, 40),
													A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10)
												]),
											_List_fromArray(
												[
													A2(
													$mdgriffith$elm_ui$Element$link,
													_List_Nil,
													{
														label: $mdgriffith$elm_ui$Element$text('> Write a letter.'),
														url: $author$project$Common$Urls$frontendWriteLetterUrl
													}),
													A2(
													$mdgriffith$elm_ui$Element$link,
													_List_Nil,
													{
														label: $mdgriffith$elm_ui$Element$text('> Start a chat.'),
														url: $author$project$Common$Urls$configChatUrl
													}),
													A2(
													$mdgriffith$elm_ui$Element$link,
													_List_Nil,
													{
														label: $mdgriffith$elm_ui$Element$text('> How does it work?'),
														url: $author$project$Common$Urls$aboutUrl
													})
												]))
										]));
							case 'About':
								return $mdgriffith$elm_ui$Element$text('About page');
							case 'ReadLetter':
								var id = _v0.a;
								return $author$project$Views$ReadLetter$view(model);
							case 'WriteLetter':
								return $author$project$Views$WriteLetter$view(model);
							case 'Chat':
								var chatId = _v0.a;
								return $author$project$Views$Chat$view(model);
							case 'ConfigChat':
								return $author$project$Views$ConfigChat$view(model);
							default:
								return $mdgriffith$elm_ui$Element$text('404');
						}
					}()))
			]),
		title: 'Hideout'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$CoreTypes$UrlChanged, onUrlRequest: $author$project$CoreTypes$UrlRequested, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));