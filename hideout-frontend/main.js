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

	/**_UNUSED/
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

	/**/
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

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
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

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


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



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


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

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
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

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
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


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
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
	if (region.h.ak === region.f.ak)
	{
		return 'on line ' + region.h.ak;
	}
	return 'on lines ' + region.h.ak + ' through ' + region.f.ak;
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



/**_UNUSED/
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

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

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
		impl.dv,
		impl.fc,
		impl.eJ,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
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


function _Platform_export(exports)
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


function _Platform_export_UNUSED(exports)
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

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
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

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
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
		K: func(record.K),
		bb: record.bb,
		a6: record.a6
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
		var message = !tag ? value : tag < 3 ? value.a : value.K;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bb;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.a6) && event.preventDefault(),
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
		impl.dv,
		impl.fc,
		impl.eJ,
		function(sendToApp, initialModel) {
			var view = impl.fd;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
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
		impl.dv,
		impl.fc,
		impl.eJ,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.a9 && impl.a9(sendToApp)
			var view = impl.fd;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aa);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.cf) && (_VirtualDom_doc.title = title = doc.cf);
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
	var onUrlChange = impl.d1;
	var onUrlRequest = impl.d2;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		a9: function(sendToApp)
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
							&& curr.b$ === next.b$
							&& curr.bD === next.bD
							&& curr.bX.a === next.bX.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		dv: function(flags)
		{
			return A3(impl.dv, flags, _Browser_getUrl(), key);
		},
		fd: impl.fd,
		fc: impl.fc,
		eJ: impl.eJ
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
		? { dm: 'hidden', cV: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { dm: 'mozHidden', cV: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { dm: 'msHidden', cV: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { dm: 'webkitHidden', cV: 'webkitvisibilitychange' }
		: { dm: 'hidden', cV: 'visibilitychange' };
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
		em: _Browser_getScene(),
		fe: {
			fi: _Browser_window.pageXOffset,
			fj: _Browser_window.pageYOffset,
			at: _Browser_doc.documentElement.clientWidth,
			bz: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		at: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bz: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			em: {
				at: node.scrollWidth,
				bz: node.scrollHeight
			},
			fe: {
				fi: node.scrollLeft,
				fj: node.scrollTop,
				at: node.clientWidth,
				bz: node.clientHeight
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
			em: _Browser_getScene(),
			fe: {
				fi: x,
				fj: y,
				at: _Browser_doc.documentElement.clientWidth,
				bz: _Browser_doc.documentElement.clientHeight
			},
			db: {
				fi: x + rect.left,
				fj: y + rect.top,
				at: rect.width,
				bz: rect.height
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
			callback(toTask(request.aF.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.aF.b, xhr)); });
		$elm$core$Maybe$isJust(request.bf) && _Http_track(router, xhr, request.bf.a);

		try {
			xhr.open(request.a3, request.cj, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.cj));
		}

		_Http_configureRequest(xhr, request);

		request.aa.a && xhr.setRequestHeader('Content-Type', request.aa.a);
		xhr.send(request.aa.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.aY; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.be.a || 0;
	xhr.responseType = request.aF.d;
	xhr.withCredentials = request.cB;
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
		cj: xhr.responseURL,
		eB: xhr.status,
		eC: xhr.statusText,
		aY: _Http_parseHeaders(xhr.getAllResponseHeaders())
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
			er: event.loaded,
			ca: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			ef: event.loaded,
			ca: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
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


function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
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
	if (options.dU) { flags += 'm'; }
	if (options.cU) { flags += 'i'; }

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
	return {$: 1, a: a};
};
var $author$project$CoreTypes$UrlRequested = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
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
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
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
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
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
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
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
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
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
		return {$: 0, a: a, b: b, c: c, d: d};
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
	return {$: 1, a: a};
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
	return {$: 0, a: a};
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
		if (!builder.k) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.m),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.m);
		} else {
			var treeLen = builder.k * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.o) : builder.o;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.k);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.m) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.m);
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
					{o: nodeList, k: (len / $elm$core$Array$branchFactor) | 0, m: tail});
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
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
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
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bx: fragment, bD: host, bV: path, bX: port_, b$: protocol, b0: query};
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
					if (_v1.$ === 1) {
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
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
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
		var task = _v0;
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
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$CoreTypes$ErrGetHost = {$: 0};
var $author$project$CoreTypes$Normal = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$CoreTypes$InitFlag = F2(
	function (protocol, host) {
		return {bD: host, b$: protocol};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$CoreTypes$initFlagDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$CoreTypes$InitFlag,
	A2($elm$json$Json$Decode$field, 'protocol', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'host', $elm$json$Json$Decode$string));
var $author$project$Letter$Init = {$: 0};
var $author$project$Chat$NotChatting = {$: 5};
var $author$project$Letter$NotSent = {$: 0};
var $author$project$CoreTypes$NotSpawned_Disp = {$: 0};
var $author$project$CoreTypes$NotSpawned_Persist = {$: 0};
var $author$project$Chat$OpeningWs = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Events$Visible = 0;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$CoreTypes$GotReadLetterResp = function (a) {
	return {$: 6, a: a};
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
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
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
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
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
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
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
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
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
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
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
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
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
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
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
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
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
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
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
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
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
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
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
				if (_v4.$ === -1) {
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
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
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
						if (_v7.$ === -1) {
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
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
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
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.eB));
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
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {b3: reqs, cc: subs};
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
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
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
							var _v4 = req.bf;
							if (_v4.$ === 1) {
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
			A3($elm$http$Http$updateReqs, router, cmds, state.b3));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
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
					state.cc)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					cB: r.cB,
					aa: r.aa,
					aF: A2(_Http_mapExpect, func, r.aF),
					aY: r.aY,
					a3: r.a3,
					be: r.be,
					bf: r.bf,
					cj: r.cj
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
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
			{cB: false, aa: r.aa, aF: r.aF, aY: r.aY, a3: r.a3, be: r.be, bf: r.bf, cj: r.cj}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{aa: $elm$http$Http$emptyBody, aF: r.aF, aY: _List_Nil, a3: 'GET', be: $elm$core$Maybe$Nothing, bf: $elm$core$Maybe$Nothing, cj: r.cj});
};
var $author$project$Letter$LetterMeta = F2(
	function (letter, readCount) {
		return {dL: letter, ee: readCount};
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Letter$Letter = F2(
	function (body, maxReadCount) {
		return {aa: body, am: maxReadCount};
	});
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
			aF: A2($elm$http$Http$expectJson, $author$project$CoreTypes$GotReadLetterResp, $author$project$Letter$letterMetaJsonDec),
			cj: $author$project$Common$Urls$backendReadLetterUrl + ('/' + letterId)
		});
};
var $author$project$Route$NotFound = {$: 6};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {R: frag, W: params, O: unvisited, bg: value, Z: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.O;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.bg);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.bg);
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
		if (maybeList.$ === 1) {
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
			if (_v2.$ === 1) {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 1) {
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
	if (maybeQuery.$ === 1) {
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
		var parser = _v0;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.bV),
					$elm$url$Url$Parser$prepareQuery(url.b0),
					url.bx,
					$elm$core$Basics$identity)));
	});
var $author$project$Route$About = function (a) {
	return {$: 1, a: a};
};
var $author$project$Route$Chat = function (a) {
	return {$: 4, a: a};
};
var $author$project$Route$ConfigChat = {$: 5};
var $author$project$Route$ReadLetter = function (a) {
	return {$: 2, a: a};
};
var $author$project$Route$Root = {$: 0};
var $author$project$Route$WriteLetter = {$: 3};
var $elm$url$Url$Parser$Parser = $elm$core$Basics$identity;
var $elm$url$Url$Parser$fragment = function (toFrag) {
	return function (_v0) {
		var visited = _v0.Z;
		var unvisited = _v0.O;
		var params = _v0.W;
		var frag = _v0.R;
		var value = _v0.bg;
		return _List_fromArray(
			[
				A5(
				$elm$url$Url$Parser$State,
				visited,
				unvisited,
				params,
				frag,
				value(
					toFrag(frag)))
			]);
	};
};
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.Z;
		var unvisited = _v0.O;
		var params = _v0.W;
		var frag = _v0.R;
		var value = _v0.bg;
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
		var parseArg = _v0;
		return function (_v1) {
			var visited = _v1.Z;
			var unvisited = _v1.O;
			var params = _v1.W;
			var frag = _v1.R;
			var value = _v1.bg;
			return A2(
				$elm$core$List$map,
				$elm$url$Url$Parser$mapState(value),
				parseArg(
					A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
		};
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
	return function (state) {
		return A2(
			$elm$core$List$concatMap,
			function (_v0) {
				var parser = _v0;
				return parser(state);
			},
			parsers);
	};
};
var $elm$url$Url$Parser$s = function (str) {
	return function (_v0) {
		var visited = _v0.Z;
		var unvisited = _v0.O;
		var params = _v0.W;
		var frag = _v0.R;
		var value = _v0.bg;
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
	};
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0;
		var parseAfter = _v1;
		return function (state) {
			return A2(
				$elm$core$List$concatMap,
				parseAfter,
				parseBefore(state));
		};
	});
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return function (_v0) {
			var visited = _v0.Z;
			var unvisited = _v0.O;
			var params = _v0.W;
			var frag = _v0.R;
			var value = _v0.bg;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				var _v2 = stringToSomething(next);
				if (!_v2.$) {
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
		};
	});
var $elm$url$Url$Parser$string = A2($elm$url$Url$Parser$custom, 'STRING', $elm$core$Maybe$Just);
var $elm$url$Url$Parser$top = function (state) {
	return _List_fromArray(
		[state]);
};
var $author$project$Views$About$None = 10;
var $pzp1997$assoc_list$AssocList$D = $elm$core$Basics$identity;
var $pzp1997$assoc_list$AssocList$get = F2(
	function (targetKey, _v0) {
		get:
		while (true) {
			var alist = _v0;
			if (!alist.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var _v2 = alist.a;
				var key = _v2.a;
				var value = _v2.b;
				var rest = alist.b;
				if (_Utils_eq(key, targetKey)) {
					return $elm$core$Maybe$Just(value);
				} else {
					var $temp$targetKey = targetKey,
						$temp$_v0 = rest;
					targetKey = $temp$targetKey;
					_v0 = $temp$_v0;
					continue get;
				}
			}
		}
	});
var $author$project$Views$About$Func_Prog = 9;
var $author$project$Views$About$Hideout_Vs_Apps = 6;
var $author$project$Views$About$How_Private = 3;
var $author$project$Views$About$Persist_Chat = 4;
var $author$project$Views$About$Self_Hosting = 8;
var $author$project$Views$About$Threat_Model = 2;
var $author$project$Views$About$Troubleshooting = 5;
var $author$project$Views$About$Use_Cases = 1;
var $author$project$Views$About$Why_Another_Disp = 7;
var $author$project$Views$About$Why_Privacy = 0;
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
var $elm$core$Basics$neq = _Utils_notEqual;
var $pzp1997$assoc_list$AssocList$remove = F2(
	function (targetKey, _v0) {
		var alist = _v0;
		return A2(
			$elm$core$List$filter,
			function (_v1) {
				var key = _v1.a;
				return !_Utils_eq(key, targetKey);
			},
			alist);
	});
var $pzp1997$assoc_list$AssocList$insert = F3(
	function (key, value, dict) {
		var _v0 = A2($pzp1997$assoc_list$AssocList$remove, key, dict);
		var alteredAlist = _v0;
		return A2(
			$elm$core$List$cons,
			_Utils_Tuple2(key, value),
			alteredAlist);
	});
var $pzp1997$assoc_list$AssocList$fromList = function (alist) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, result) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($pzp1997$assoc_list$AssocList$insert, key, value, result);
			}),
		_List_Nil,
		alist);
};
var $author$project$Views$About$urlFragAndSectionAssoc = $pzp1997$assoc_list$AssocList$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('why-privacy', 0),
			_Utils_Tuple2('use-cases', 1),
			_Utils_Tuple2('threat-model', 2),
			_Utils_Tuple2('how-private', 3),
			_Utils_Tuple2('persist-chat', 4),
			_Utils_Tuple2('troubleshooting', 5),
			_Utils_Tuple2('hideout-vs-apps', 6),
			_Utils_Tuple2('why-another-disp', 7),
			_Utils_Tuple2('self-hosting', 8),
			_Utils_Tuple2('func-prog', 9)
		]));
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Views$About$urlFragToSection = function (maybeStr) {
	if (maybeStr.$ === 1) {
		return 10;
	} else {
		var str = maybeStr.a;
		return A2(
			$elm$core$Maybe$withDefault,
			10,
			A2($pzp1997$assoc_list$AssocList$get, str, $author$project$Views$About$urlFragAndSectionAssoc));
	}
};
var $author$project$Route$routeParser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2($elm$url$Url$Parser$map, $author$project$Route$Root, $elm$url$Url$Parser$top),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$About,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('about'),
				$elm$url$Url$Parser$fragment($author$project$Views$About$urlFragToSection))),
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
var $author$project$Route$getRoute = function (url) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Route$NotFound,
		A2($elm$url$Url$Parser$parse, $author$project$Route$routeParser, url));
};
var $author$project$CoreTypes$GotViewport = function (a) {
	return {$: 2, a: a};
};
var $elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var $author$project$Main$getViewportCmd = A2($elm$core$Task$perform, $author$project$CoreTypes$GotViewport, $elm$browser$Browser$Dom$getViewport);
var $author$project$Views$About$init = {b7: 10};
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$port_InitWs = _Platform_outgoingPort('port_InitWs', $elm$json$Json$Encode$string);
var $joneshf$elm_tagged$Tagged$Tagged = $elm$core$Basics$identity;
var $joneshf$elm_tagged$Tagged$tag = $elm$core$Basics$identity;
var $author$project$Main$updateAboutPageModelWithRoute = F2(
	function (route, model) {
		var previousSection = model.b7;
		return _Utils_update(
			model,
			{
				b7: function () {
					if (route.$ === 1) {
						var section = route.a;
						if (section === 10) {
							return previousSection;
						} else {
							return section;
						}
					} else {
						return previousSection;
					}
				}()
			});
	});
var $author$project$Main$initModel = F3(
	function (initFlag, url, navKey) {
		var route = $author$project$Route$getRoute(url);
		return _Utils_Tuple2(
			{
				cq: A2($author$project$Main$updateAboutPageModelWithRoute, route, $author$project$Views$About$init),
				cX: function () {
					if (route.$ === 4) {
						var chatIdStr = route.a;
						return $author$project$Chat$OpeningWs(
							$joneshf$elm_tagged$Tagged$tag(chatIdStr));
					} else {
						return $author$project$Chat$NotChatting;
					}
				}(),
				c9: '2',
				bD: initFlag.bD,
				dF: false,
				dH: '',
				dM: true,
				dN: {aa: '', am: '1'},
				dO: {ed: $author$project$Letter$Init, fh: $author$project$Letter$NotSent},
				dX: navKey,
				d5: initFlag.b$ + ('//' + initFlag.bD),
				d9: '2',
				b$: initFlag.b$,
				ej: route,
				ey: $author$project$CoreTypes$NotSpawned_Disp,
				ez: $author$project$CoreTypes$NotSpawned_Persist,
				eQ: '',
				ce: $elm$time$Time$millisToPosix(0),
				fe: {
					em: {bz: 1080, at: 1920},
					fe: {bz: 1080, at: 1920, fi: 0, fj: 0}
				},
				ff: 0
			},
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$author$project$Main$getViewportCmd,
						function () {
						switch (route.$) {
							case 4:
								var chatIdStr = route.a;
								return $author$project$Main$port_InitWs(chatIdStr);
							case 2:
								var letterId = route.a;
								return $author$project$Main$getLetterReq(letterId);
							default:
								return $elm$core$Platform$Cmd$none;
						}
					}()
					])));
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $author$project$Main$init = F3(
	function (jsonFlag, url, navKey) {
		var tryInitModel = A2(
			$elm$core$Result$map,
			function (initFlag) {
				return A3($author$project$Main$initModel, initFlag, url, navKey);
			},
			A2($elm$json$Json$Decode$decodeValue, $author$project$CoreTypes$initFlagDecoder, jsonFlag));
		if (!tryInitModel.$) {
			var _v1 = tryInitModel.a;
			var model = _v1.a;
			var cmd = _v1.b;
			return _Utils_Tuple2(
				$author$project$CoreTypes$Normal(model),
				cmd);
		} else {
			var decErr = tryInitModel.a;
			return _Utils_Tuple2($author$project$CoreTypes$ErrGetHost, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$CoreTypes$ChatElmMsg = function (a) {
	return {$: 18, a: a};
};
var $author$project$Chat$GotTime = function (a) {
	return {$: 15, a: a};
};
var $author$project$CoreTypes$GotTime = function (a) {
	return {$: 23, a: a};
};
var $author$project$CoreTypes$OnKeyDown = function (a) {
	return {$: 21, a: a};
};
var $author$project$CoreTypes$OnKeyUp = function (a) {
	return {$: 22, a: a};
};
var $author$project$CoreTypes$OnVisibilityChange = function (a) {
	return {$: 20, a: a};
};
var $author$project$CoreTypes$OnWindowResized = {$: 19};
var $author$project$Chat$OnWsError = {$: 13};
var $author$project$Chat$OnWsMsg = function (a) {
	return {$: 14, a: a};
};
var $author$project$Chat$OnWsReady = function (a) {
	return {$: 12, a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {b_: processes, cd: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 1) {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
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
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.b_;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.cd);
		if (_v0.$ === 1) {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {bW: pids, cc: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
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
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {bt: event, bJ: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
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
			state.bW,
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
		var key = _v0.bJ;
		var event = _v0.bt;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.cc);
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
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, 0, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, 0, 'keyup');
var $elm$browser$Browser$Events$Window = 1;
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		1,
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
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$browser$Browser$Events$Hidden = 1;
var $elm$browser$Browser$Events$withHidden = F2(
	function (func, isHidden) {
		return func(
			isHidden ? 1 : 0);
	});
var $elm$browser$Browser$Events$onVisibilityChange = function (func) {
	var info = _Browser_visibilityInfo(0);
	return A3(
		$elm$browser$Browser$Events$on,
		0,
		info.cV,
		A2(
			$elm$json$Json$Decode$map,
			$elm$browser$Browser$Events$withHidden(func),
			A2(
				$elm$json$Json$Decode$field,
				'target',
				A2($elm$json$Json$Decode$field, info.dm, $elm$json$Json$Decode$bool))));
};
var $author$project$Main$port_RecvWsMsg = _Platform_incomingPort('port_RecvWsMsg', $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $author$project$Main$port_WsError = _Platform_incomingPort(
	'port_WsError',
	$elm$json$Json$Decode$null(0));
var $author$project$Main$port_WsReady = _Platform_incomingPort('port_WsReady', $elm$json$Json$Decode$string);
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$port_WsReady(
				A2($elm$core$Basics$composeL, $author$project$CoreTypes$ChatElmMsg, $author$project$Chat$OnWsReady)),
				$author$project$Main$port_WsError(
				function (_v1) {
					return $author$project$CoreTypes$ChatElmMsg($author$project$Chat$OnWsError);
				}),
				$author$project$Main$port_RecvWsMsg(
				A2($elm$core$Basics$composeL, $author$project$CoreTypes$ChatElmMsg, $author$project$Chat$OnWsMsg)),
				$elm$browser$Browser$Events$onResize(
				F2(
					function (_v2, _v3) {
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
					A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string))),
				$elm$browser$Browser$Events$onVisibilityChange($author$project$CoreTypes$OnVisibilityChange),
				A2($elm$time$Time$every, 1000, $author$project$CoreTypes$GotTime),
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$CoreTypes$ChatElmMsg,
				A2($elm$time$Time$every, 1000, $author$project$Chat$GotTime))
			]));
};
var $author$project$Letter$Got = function (a) {
	return {$: 2, a: a};
};
var $author$project$CoreTypes$GotChatId = function (a) {
	return {$: 3, a: a};
};
var $author$project$CoreTypes$GotError_Disp = function (a) {
	return {$: 2, a: a};
};
var $author$project$CoreTypes$GotError_Persist = function (a) {
	return {$: 2, a: a};
};
var $author$project$CoreTypes$GotLetterId = function (a) {
	return {$: 3, a: a};
};
var $author$project$CoreTypes$GotLetterSendResp = function (a) {
	return {$: 11, a: a};
};
var $author$project$Letter$GotResp = function (a) {
	return {$: 2, a: a};
};
var $author$project$CoreTypes$GotSpawnDispChatResp = function (a) {
	return {$: 14, a: a};
};
var $author$project$CoreTypes$GotSpawnPersistChatResp = function (a) {
	return {$: 17, a: a};
};
var $author$project$Letter$Sent = function (a) {
	return {$: 1, a: a};
};
var $author$project$CoreTypes$Waiting_Disp = {$: 1};
var $author$project$CoreTypes$Waiting_Persist = {$: 1};
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
var $author$project$Common$Urls$frontendChatUrl = function (chatId) {
	return A2(
		$elm$url$Url$Builder$absolute,
		_List_fromArray(
			['chat', chatId]),
		_List_Nil);
};
var $author$project$Chat$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$not = _Basics_not;
var $author$project$Chat$Content = 0;
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
			_Json_emptyObject(0),
			pairs));
};
var $joneshf$elm_tagged$Tagged$untag = function (_v0) {
	var x = _v0;
	return x;
};
var $author$project$Chat$mkWsMsg = F2(
	function (msgType, msgBody) {
		var msgTypeStr = function () {
			switch (msgType) {
				case 0:
					return 'content';
				case 1:
					return 'join';
				case 2:
					return 'nameChange';
				case 3:
					return 'typeHint';
				default:
					return 'leave';
			}
		}();
		return A2(
			$elm$json$Json$Encode$encode,
			0,
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'msgType',
						$elm$json$Json$Encode$string(msgTypeStr)),
						_Utils_Tuple2(
						'msgBody',
						$elm$json$Json$Encode$string(
							$joneshf$elm_tagged$Tagged$untag(msgBody)))
					])));
	});
var $author$project$Chat$mkContentMsg = $author$project$Chat$mkWsMsg(0);
var $author$project$Views$Chat$port_SendWsMsg = _Platform_outgoingPort('port_SendWsMsg', $elm$json$Json$Encode$string);
var $author$project$Views$Chat$sendChatMsg = function (msgBody) {
	return $elm$core$String$isEmpty(
		$joneshf$elm_tagged$Tagged$untag(msgBody)) ? $elm$core$Platform$Cmd$none : $author$project$Views$Chat$port_SendWsMsg(
		$author$project$Chat$mkContentMsg(msgBody));
};
var $author$project$Views$Chat$handleKeyDown = F2(
	function (status, key) {
		if (!status.$) {
			var model = status.a;
			switch (key) {
				case 'Enter':
					return _Utils_Tuple2(
						$author$project$Chat$Normal(model),
						((!model.dF) && model.dE) ? $author$project$Views$Chat$sendChatMsg(model.dw) : $elm$core$Platform$Cmd$none);
				case 'Shift':
					return _Utils_Tuple2(
						$author$project$Chat$Normal(
							_Utils_update(
								model,
								{dF: true})),
						$elm$core$Platform$Cmd$none);
				default:
					return _Utils_Tuple2(status, $elm$core$Platform$Cmd$none);
			}
		} else {
			return _Utils_Tuple2(status, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Views$Chat$handleKeyUp = F2(
	function (status, key) {
		if (!status.$) {
			var model = status.a;
			if (key === 'Shift') {
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{dF: false})),
					$elm$core$Platform$Cmd$none);
			} else {
				return _Utils_Tuple2(status, $elm$core$Platform$Cmd$none);
			}
		} else {
			return _Utils_Tuple2(status, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $author$project$Utils$Types$Bad = function (a) {
	return {$: 1, a: a};
};
var $author$project$Utils$Types$Good = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Utils$Types$strToPosIntInput = function (str) {
	var _v0 = $elm$core$String$toInt(str);
	if (_v0.$ === 1) {
		return $author$project$Utils$Types$Bad(str);
	} else {
		var n = _v0.a;
		return (n >= 1) ? $author$project$Utils$Types$Good(n) : $author$project$Utils$Types$Bad(str);
	}
};
var $elm$http$Http$stringBody = _Http_pair;
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 1) {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 1) {
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
		var _v0 = url.b$;
		if (!_v0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.bx,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.b0,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.bX,
					_Utils_ap(http, url.bD)),
				url.bV)));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
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
var $author$project$Views$About$update = F2(
	function (msg, model) {
		if (!msg.$) {
			var section = msg.a;
			return _Utils_update(
				model,
				{b7: section});
		} else {
			return _Utils_update(
				model,
				{b7: 10});
		}
	});
var $author$project$Chat$NotChanging = {$: 0};
var $author$project$Chat$NotTyping = {$: 1};
var $author$project$Chat$WsError = {$: 2};
var $author$project$Emoji$allHex = _List_fromArray(
	['0023-FE0F-20E3', '002A-FE0F-20E3', '0030-FE0F-20E3', '0031-FE0F-20E3', '0032-FE0F-20E3', '0033-FE0F-20E3', '0034-FE0F-20E3', '0035-FE0F-20E3', '0036-FE0F-20E3', '0037-FE0F-20E3', '0038-FE0F-20E3', '0039-FE0F-20E3', '00A9', '00AE', '1F004', '1F0CF', '1F10D', '1F10E', '1F10F', '1F12F', '1F16D', '1F16E', '1F16F', '1F170', '1F171', '1F17E', '1F17F', '1F18E', '1F191', '1F192', '1F193', '1F194', '1F195', '1F196', '1F197', '1F198', '1F199', '1F19A', '1F1E6-1F1E8', '1F1E6-1F1E9', '1F1E6-1F1EA', '1F1E6-1F1EB', '1F1E6-1F1EC', '1F1E6-1F1EE', '1F1E6-1F1F1', '1F1E6-1F1F2', '1F1E6-1F1F4', '1F1E6-1F1F6', '1F1E6-1F1F7', '1F1E6-1F1F8', '1F1E6-1F1F9', '1F1E6-1F1FA', '1F1E6-1F1FC', '1F1E6-1F1FD', '1F1E6-1F1FF', '1F1E7-1F1E6', '1F1E7-1F1E7', '1F1E7-1F1E9', '1F1E7-1F1EA', '1F1E7-1F1EB', '1F1E7-1F1EC', '1F1E7-1F1ED', '1F1E7-1F1EE', '1F1E7-1F1EF', '1F1E7-1F1F1', '1F1E7-1F1F2', '1F1E7-1F1F3', '1F1E7-1F1F4', '1F1E7-1F1F6', '1F1E7-1F1F7', '1F1E7-1F1F8', '1F1E7-1F1F9', '1F1E7-1F1FB', '1F1E7-1F1FC', '1F1E7-1F1FE', '1F1E7-1F1FF', '1F1E8-1F1E6', '1F1E8-1F1E8', '1F1E8-1F1E9', '1F1E8-1F1EB', '1F1E8-1F1EC', '1F1E8-1F1ED', '1F1E8-1F1EE', '1F1E8-1F1F0', '1F1E8-1F1F1', '1F1E8-1F1F2', '1F1E8-1F1F3', '1F1E8-1F1F4', '1F1E8-1F1F5', '1F1E8-1F1F7', '1F1E8-1F1FA', '1F1E8-1F1FB', '1F1E8-1F1FC', '1F1E8-1F1FD', '1F1E8-1F1FE', '1F1E8-1F1FF', '1F1E9-1F1EA', '1F1E9-1F1EC', '1F1E9-1F1EF', '1F1E9-1F1F0', '1F1E9-1F1F2', '1F1E9-1F1F4', '1F1E9-1F1FF', '1F1EA-1F1E6', '1F1EA-1F1E8', '1F1EA-1F1EA', '1F1EA-1F1EC', '1F1EA-1F1ED', '1F1EA-1F1F7', '1F1EA-1F1F8', '1F1EA-1F1F9', '1F1EA-1F1FA', '1F1EB-1F1EE', '1F1EB-1F1EF', '1F1EB-1F1F0', '1F1EB-1F1F2', '1F1EB-1F1F4', '1F1EB-1F1F7', '1F1EC-1F1E6', '1F1EC-1F1E7', '1F1EC-1F1E9', '1F1EC-1F1EA', '1F1EC-1F1EB', '1F1EC-1F1EC', '1F1EC-1F1ED', '1F1EC-1F1EE', '1F1EC-1F1F1', '1F1EC-1F1F2', '1F1EC-1F1F3', '1F1EC-1F1F5', '1F1EC-1F1F6', '1F1EC-1F1F7', '1F1EC-1F1F8', '1F1EC-1F1F9', '1F1EC-1F1FA', '1F1EC-1F1FC', '1F1EC-1F1FE', '1F1ED-1F1F0', '1F1ED-1F1F2', '1F1ED-1F1F3', '1F1ED-1F1F7', '1F1ED-1F1F9', '1F1ED-1F1FA', '1F1EE-1F1E8', '1F1EE-1F1E9', '1F1EE-1F1EA', '1F1EE-1F1F1', '1F1EE-1F1F2', '1F1EE-1F1F3', '1F1EE-1F1F4', '1F1EE-1F1F6', '1F1EE-1F1F7', '1F1EE-1F1F8', '1F1EE-1F1F9', '1F1EF-1F1EA', '1F1EF-1F1F2', '1F1EF-1F1F4', '1F1EF-1F1F5', '1F1F0-1F1EA', '1F1F0-1F1EC', '1F1F0-1F1ED', '1F1F0-1F1EE', '1F1F0-1F1F2', '1F1F0-1F1F3', '1F1F0-1F1F5', '1F1F0-1F1F7', '1F1F0-1F1FC', '1F1F0-1F1FE', '1F1F0-1F1FF', '1F1F1-1F1E6', '1F1F1-1F1E7', '1F1F1-1F1E8', '1F1F1-1F1EE', '1F1F1-1F1F0', '1F1F1-1F1F7', '1F1F1-1F1F8', '1F1F1-1F1F9', '1F1F1-1F1FA', '1F1F1-1F1FB', '1F1F1-1F1FE', '1F1F2-1F1E6', '1F1F2-1F1E8', '1F1F2-1F1E9', '1F1F2-1F1EA', '1F1F2-1F1EB', '1F1F2-1F1EC', '1F1F2-1F1ED', '1F1F2-1F1F0', '1F1F2-1F1F1', '1F1F2-1F1F2', '1F1F2-1F1F3', '1F1F2-1F1F4', '1F1F2-1F1F5', '1F1F2-1F1F6', '1F1F2-1F1F7', '1F1F2-1F1F8', '1F1F2-1F1F9', '1F1F2-1F1FA', '1F1F2-1F1FB', '1F1F2-1F1FC', '1F1F2-1F1FD', '1F1F2-1F1FE', '1F1F2-1F1FF', '1F1F3-1F1E6', '1F1F3-1F1E8', '1F1F3-1F1EA', '1F1F3-1F1EB', '1F1F3-1F1EC', '1F1F3-1F1EE', '1F1F3-1F1F1', '1F1F3-1F1F4', '1F1F3-1F1F5', '1F1F3-1F1F7', '1F1F3-1F1FA', '1F1F3-1F1FF', '1F1F4-1F1F2', '1F1F5-1F1E6', '1F1F5-1F1EA', '1F1F5-1F1EB', '1F1F5-1F1EC', '1F1F5-1F1ED', '1F1F5-1F1F0', '1F1F5-1F1F1', '1F1F5-1F1F2', '1F1F5-1F1F3', '1F1F5-1F1F7', '1F1F5-1F1F8', '1F1F5-1F1F9', '1F1F5-1F1FC', '1F1F5-1F1FE', '1F1F6-1F1E6', '1F1F7-1F1EA', '1F1F7-1F1F4', '1F1F7-1F1F8', '1F1F7-1F1FA', '1F1F7-1F1FC', '1F1F8-1F1E6', '1F1F8-1F1E7', '1F1F8-1F1E8', '1F1F8-1F1E9', '1F1F8-1F1EA', '1F1F8-1F1EC', '1F1F8-1F1ED', '1F1F8-1F1EE', '1F1F8-1F1EF', '1F1F8-1F1F0', '1F1F8-1F1F1', '1F1F8-1F1F2', '1F1F8-1F1F3', '1F1F8-1F1F4', '1F1F8-1F1F7', '1F1F8-1F1F8', '1F1F8-1F1F9', '1F1F8-1F1FB', '1F1F8-1F1FD', '1F1F8-1F1FE', '1F1F8-1F1FF', '1F1F9-1F1E6', '1F1F9-1F1E8', '1F1F9-1F1E9', '1F1F9-1F1EB', '1F1F9-1F1EC', '1F1F9-1F1ED', '1F1F9-1F1EF', '1F1F9-1F1F0', '1F1F9-1F1F1', '1F1F9-1F1F2', '1F1F9-1F1F3', '1F1F9-1F1F4', '1F1F9-1F1F7', '1F1F9-1F1F9', '1F1F9-1F1FB', '1F1F9-1F1FC', '1F1F9-1F1FF', '1F1FA-1F1E6', '1F1FA-1F1EC', '1F1FA-1F1F2', '1F1FA-1F1F3', '1F1FA-1F1F8', '1F1FA-1F1FE', '1F1FA-1F1FF', '1F1FB-1F1E6', '1F1FB-1F1E8', '1F1FB-1F1EA', '1F1FB-1F1EC', '1F1FB-1F1EE', '1F1FB-1F1F3', '1F1FB-1F1FA', '1F1FC-1F1EB', '1F1FC-1F1F8', '1F1FD-1F1F0', '1F1FE-1F1EA', '1F1FE-1F1F9', '1F1FF-1F1E6', '1F1FF-1F1F2', '1F1FF-1F1FC', '1F201', '1F202', '1F21A', '1F22F', '1F232', '1F233', '1F234', '1F235', '1F236', '1F237', '1F238', '1F239', '1F23A', '1F250', '1F251', '1F300', '1F301', '1F302', '1F303', '1F304', '1F305', '1F306', '1F307', '1F308', '1F309', '1F30A', '1F30B', '1F30C', '1F30D', '1F30E', '1F30F', '1F310', '1F311', '1F312', '1F313', '1F314', '1F315', '1F316', '1F317', '1F318', '1F319', '1F31A', '1F31B', '1F31C', '1F31D', '1F31E', '1F31F', '1F320', '1F321', '1F324', '1F325', '1F326', '1F327', '1F328', '1F329', '1F32A', '1F32B', '1F32C', '1F32D', '1F32E', '1F32F', '1F330', '1F331', '1F332', '1F333', '1F334', '1F335', '1F336', '1F337', '1F338', '1F339', '1F33A', '1F33B', '1F33C', '1F33D', '1F33E', '1F33F', '1F340', '1F341', '1F342', '1F343', '1F344', '1F345', '1F346', '1F347', '1F348', '1F349', '1F34A', '1F34B', '1F34C', '1F34D', '1F34E', '1F34F', '1F350', '1F351', '1F352', '1F353', '1F354', '1F355', '1F356', '1F357', '1F358', '1F359', '1F35A', '1F35B', '1F35C', '1F35D', '1F35E', '1F35F', '1F360', '1F361', '1F362', '1F363', '1F364', '1F365', '1F366', '1F367', '1F368', '1F369', '1F36A', '1F36B', '1F36C', '1F36D', '1F36E', '1F36F', '1F370', '1F371', '1F372', '1F373', '1F374', '1F375', '1F376', '1F377', '1F378', '1F379', '1F37A', '1F37B', '1F37C', '1F37D', '1F37E', '1F37F', '1F380', '1F381', '1F382', '1F383', '1F384', '1F385-1F3FB', '1F385-1F3FC', '1F385-1F3FD', '1F385-1F3FE', '1F385-1F3FF', '1F385', '1F386', '1F387', '1F388', '1F389', '1F38A', '1F38B', '1F38C', '1F38D', '1F38E', '1F38F', '1F390', '1F391', '1F392', '1F393', '1F396', '1F397', '1F399', '1F39A', '1F39B', '1F39E', '1F39F', '1F3A0', '1F3A1', '1F3A2', '1F3A3', '1F3A4', '1F3A5', '1F3A6', '1F3A7', '1F3A8', '1F3A9', '1F3AA', '1F3AB', '1F3AC', '1F3AD', '1F3AE', '1F3AF', '1F3B0', '1F3B1', '1F3B2', '1F3B3', '1F3B4', '1F3B5', '1F3B6', '1F3B7', '1F3B8', '1F3B9', '1F3BA', '1F3BB', '1F3BC', '1F3BD', '1F3BE', '1F3BF', '1F3C0', '1F3C1', '1F3C2-1F3FB', '1F3C2-1F3FC', '1F3C2-1F3FD', '1F3C2-1F3FE', '1F3C2-1F3FF', '1F3C2', '1F3C3-1F3FB-200D-2640-FE0F', '1F3C3-1F3FB-200D-2642-FE0F', '1F3C3-1F3FB', '1F3C3-1F3FC-200D-2640-FE0F', '1F3C3-1F3FC-200D-2642-FE0F', '1F3C3-1F3FC', '1F3C3-1F3FD-200D-2640-FE0F', '1F3C3-1F3FD-200D-2642-FE0F', '1F3C3-1F3FD', '1F3C3-1F3FE-200D-2640-FE0F', '1F3C3-1F3FE-200D-2642-FE0F', '1F3C3-1F3FE', '1F3C3-1F3FF-200D-2640-FE0F', '1F3C3-1F3FF-200D-2642-FE0F', '1F3C3-1F3FF', '1F3C3-200D-2640-FE0F', '1F3C3-200D-2642-FE0F', '1F3C3', '1F3C4-1F3FB-200D-2640-FE0F', '1F3C4-1F3FB-200D-2642-FE0F', '1F3C4-1F3FB', '1F3C4-1F3FC-200D-2640-FE0F', '1F3C4-1F3FC-200D-2642-FE0F', '1F3C4-1F3FC', '1F3C4-1F3FD-200D-2640-FE0F', '1F3C4-1F3FD-200D-2642-FE0F', '1F3C4-1F3FD', '1F3C4-1F3FE-200D-2640-FE0F', '1F3C4-1F3FE-200D-2642-FE0F', '1F3C4-1F3FE', '1F3C4-1F3FF-200D-2640-FE0F', '1F3C4-1F3FF-200D-2642-FE0F', '1F3C4-1F3FF', '1F3C4-200D-2640-FE0F', '1F3C4-200D-2642-FE0F', '1F3C4', '1F3C5', '1F3C6', '1F3C7-1F3FB', '1F3C7-1F3FC', '1F3C7-1F3FD', '1F3C7-1F3FE', '1F3C7-1F3FF', '1F3C7', '1F3C8', '1F3C9', '1F3CA-1F3FB-200D-2640-FE0F', '1F3CA-1F3FB-200D-2642-FE0F', '1F3CA-1F3FB', '1F3CA-1F3FC-200D-2640-FE0F', '1F3CA-1F3FC-200D-2642-FE0F', '1F3CA-1F3FC', '1F3CA-1F3FD-200D-2640-FE0F', '1F3CA-1F3FD-200D-2642-FE0F', '1F3CA-1F3FD', '1F3CA-1F3FE-200D-2640-FE0F', '1F3CA-1F3FE-200D-2642-FE0F', '1F3CA-1F3FE', '1F3CA-1F3FF-200D-2640-FE0F', '1F3CA-1F3FF-200D-2642-FE0F', '1F3CA-1F3FF', '1F3CA-200D-2640-FE0F', '1F3CA-200D-2642-FE0F', '1F3CA', '1F3CB-1F3FB-200D-2640-FE0F', '1F3CB-1F3FB-200D-2642-FE0F', '1F3CB-1F3FB', '1F3CB-1F3FC-200D-2640-FE0F', '1F3CB-1F3FC-200D-2642-FE0F', '1F3CB-1F3FC', '1F3CB-1F3FD-200D-2640-FE0F', '1F3CB-1F3FD-200D-2642-FE0F', '1F3CB-1F3FD', '1F3CB-1F3FE-200D-2640-FE0F', '1F3CB-1F3FE-200D-2642-FE0F', '1F3CB-1F3FE', '1F3CB-1F3FF-200D-2640-FE0F', '1F3CB-1F3FF-200D-2642-FE0F', '1F3CB-1F3FF', '1F3CB-FE0F-200D-2640-FE0F', '1F3CB-FE0F-200D-2642-FE0F', '1F3CB', '1F3CC-1F3FB-200D-2640-FE0F', '1F3CC-1F3FB-200D-2642-FE0F', '1F3CC-1F3FB', '1F3CC-1F3FC-200D-2640-FE0F', '1F3CC-1F3FC-200D-2642-FE0F', '1F3CC-1F3FC', '1F3CC-1F3FD-200D-2640-FE0F', '1F3CC-1F3FD-200D-2642-FE0F', '1F3CC-1F3FD', '1F3CC-1F3FE-200D-2640-FE0F', '1F3CC-1F3FE-200D-2642-FE0F', '1F3CC-1F3FE', '1F3CC-1F3FF-200D-2640-FE0F', '1F3CC-1F3FF-200D-2642-FE0F', '1F3CC-1F3FF', '1F3CC-FE0F-200D-2640-FE0F', '1F3CC-FE0F-200D-2642-FE0F', '1F3CC', '1F3CD', '1F3CE', '1F3CF', '1F3D0', '1F3D1', '1F3D2', '1F3D3', '1F3D4', '1F3D5', '1F3D6', '1F3D7', '1F3D8', '1F3D9', '1F3DA', '1F3DB', '1F3DC', '1F3DD', '1F3DE', '1F3DF', '1F3E0', '1F3E1', '1F3E2', '1F3E3', '1F3E4', '1F3E5', '1F3E6', '1F3E7', '1F3E8', '1F3E9', '1F3EA', '1F3EB', '1F3EC', '1F3ED', '1F3EE', '1F3EF', '1F3F0', '1F3F3-FE0F-200D-1F308', '1F3F3-FE0F-200D-1F4CC-200D-2699-FE0F', '1F3F3-FE0F-200D-1F7E5', '1F3F3-FE0F-200D-1F7E6-200D-1F30C', '1F3F3-FE0F-200D-1F7E6', '1F3F3-FE0F-200D-1F7E7', '1F3F3-FE0F-200D-1F7E8', '1F3F3-FE0F-200D-1F7E9-200D-2B50-200D-1F7E9', '1F3F3-FE0F-200D-1F7E9', '1F3F3-FE0F-200D-1F7EA', '1F3F3-FE0F-200D-1F7EB', '1F3F3-FE0F-200D-26A7-FE0F', '1F3F3-FE0F', '1F3F3', '1F3F4-200D-2620-FE0F', '1F3F4-E0064-E0065-E0062-E0065-E007F', '1F3F4-E0064-E0065-E0062-E0079-E007F', '1F3F4-E0065-E0073-E0061-E0073-E007F', '1F3F4-E0067-E0062-E0065-E006E-E0067-E007F', '1F3F4-E0067-E0062-E0073-E0063-E0074-E007F', '1F3F4-E0067-E0062-E0077-E006C-E0073-E007F', '1F3F4-E0075-E0073-E0063-E0061-E007F', '1F3F4-E0075-E0073-E0074-E0078-E007F', '1F3F4', '1F3F5', '1F3F7', '1F3F8', '1F3F9', '1F3FA', '1F3FB', '1F3FC', '1F3FD', '1F3FE', '1F3FF', '1F400', '1F401', '1F402', '1F403', '1F404', '1F405', '1F406', '1F407', '1F408-200D-2B1B', '1F408', '1F409', '1F40A', '1F40B', '1F40C', '1F40D', '1F40E', '1F40F', '1F410', '1F411', '1F412', '1F413', '1F414', '1F415-200D-1F9BA', '1F415', '1F416', '1F417', '1F418', '1F419', '1F41A', '1F41B', '1F41C', '1F41D', '1F41E', '1F41F', '1F420', '1F421', '1F422', '1F423', '1F424', '1F425', '1F426', '1F427', '1F428', '1F429', '1F42A', '1F42B', '1F42C', '1F42D', '1F42E', '1F42F', '1F430', '1F431-200D-1F4BB', '1F431', '1F432', '1F433', '1F434', '1F435', '1F436', '1F437', '1F438', '1F439', '1F43A', '1F43B-200D-2744-FE0F', '1F43B', '1F43C', '1F43D', '1F43E', '1F43F', '1F440', '1F441-FE0F-200D-1F5E8-FE0F', '1F441', '1F442-1F3FB', '1F442-1F3FC', '1F442-1F3FD', '1F442-1F3FE', '1F442-1F3FF', '1F442', '1F443-1F3FB', '1F443-1F3FC', '1F443-1F3FD', '1F443-1F3FE', '1F443-1F3FF', '1F443', '1F444', '1F445', '1F446-1F3FB', '1F446-1F3FC', '1F446-1F3FD', '1F446-1F3FE', '1F446-1F3FF', '1F446', '1F447-1F3FB', '1F447-1F3FC', '1F447-1F3FD', '1F447-1F3FE', '1F447-1F3FF', '1F447', '1F448-1F3FB', '1F448-1F3FC', '1F448-1F3FD', '1F448-1F3FE', '1F448-1F3FF', '1F448', '1F449-1F3FB', '1F449-1F3FC', '1F449-1F3FD', '1F449-1F3FE', '1F449-1F3FF', '1F449', '1F44A-1F3FB', '1F44A-1F3FC', '1F44A-1F3FD', '1F44A-1F3FE', '1F44A-1F3FF', '1F44A', '1F44B-1F3FB', '1F44B-1F3FC', '1F44B-1F3FD', '1F44B-1F3FE', '1F44B-1F3FF', '1F44B', '1F44C-1F3FB', '1F44C-1F3FC', '1F44C-1F3FD', '1F44C-1F3FE', '1F44C-1F3FF', '1F44C', '1F44D-1F3FB', '1F44D-1F3FC', '1F44D-1F3FD', '1F44D-1F3FE', '1F44D-1F3FF', '1F44D', '1F44E-1F3FB', '1F44E-1F3FC', '1F44E-1F3FD', '1F44E-1F3FE', '1F44E-1F3FF', '1F44E', '1F44F-1F3FB', '1F44F-1F3FC', '1F44F-1F3FD', '1F44F-1F3FE', '1F44F-1F3FF', '1F44F', '1F450-1F3FB', '1F450-1F3FC', '1F450-1F3FD', '1F450-1F3FE', '1F450-1F3FF', '1F450', '1F451', '1F452', '1F453', '1F454', '1F455', '1F456', '1F457', '1F458', '1F459', '1F45A', '1F45B', '1F45C', '1F45D', '1F45E', '1F45F', '1F460', '1F461', '1F462', '1F463', '1F464', '1F465', '1F466-1F3FB', '1F466-1F3FC', '1F466-1F3FD', '1F466-1F3FE', '1F466-1F3FF', '1F466', '1F467-1F3FB', '1F467-1F3FC', '1F467-1F3FD', '1F467-1F3FE', '1F467-1F3FF', '1F467', '1F468-1F3FB-200D-1F33E', '1F468-1F3FB-200D-1F373', '1F468-1F3FB-200D-1F37C', '1F468-1F3FB-200D-1F393', '1F468-1F3FB-200D-1F3A4', '1F468-1F3FB-200D-1F3A8', '1F468-1F3FB-200D-1F3EB', '1F468-1F3FB-200D-1F3ED', '1F468-1F3FB-200D-1F4BB', '1F468-1F3FB-200D-1F4BC', '1F468-1F3FB-200D-1F527', '1F468-1F3FB-200D-1F52C', '1F468-1F3FB-200D-1F680', '1F468-1F3FB-200D-1F692', '1F468-1F3FB-200D-1F91D-200D-1F468-1F3FC', '1F468-1F3FB-200D-1F91D-200D-1F468-1F3FD', '1F468-1F3FB-200D-1F91D-200D-1F468-1F3FE', '1F468-1F3FB-200D-1F91D-200D-1F468-1F3FF', '1F468-1F3FB-200D-1F9AF', '1F468-1F3FB-200D-1F9B0', '1F468-1F3FB-200D-1F9B1', '1F468-1F3FB-200D-1F9B2', '1F468-1F3FB-200D-1F9B3', '1F468-1F3FB-200D-1F9BC', '1F468-1F3FB-200D-1F9BD', '1F468-1F3FB-200D-2695-FE0F', '1F468-1F3FB-200D-2696-FE0F', '1F468-1F3FB-200D-2708-FE0F', '1F468-1F3FB', '1F468-1F3FC-200D-1F33E', '1F468-1F3FC-200D-1F373', '1F468-1F3FC-200D-1F37C', '1F468-1F3FC-200D-1F393', '1F468-1F3FC-200D-1F3A4', '1F468-1F3FC-200D-1F3A8', '1F468-1F3FC-200D-1F3EB', '1F468-1F3FC-200D-1F3ED', '1F468-1F3FC-200D-1F4BB', '1F468-1F3FC-200D-1F4BC', '1F468-1F3FC-200D-1F527', '1F468-1F3FC-200D-1F52C', '1F468-1F3FC-200D-1F680', '1F468-1F3FC-200D-1F692', '1F468-1F3FC-200D-1F91D-200D-1F468-1F3FB', '1F468-1F3FC-200D-1F91D-200D-1F468-1F3FD', '1F468-1F3FC-200D-1F91D-200D-1F468-1F3FE', '1F468-1F3FC-200D-1F91D-200D-1F468-1F3FF', '1F468-1F3FC-200D-1F9AF', '1F468-1F3FC-200D-1F9B0', '1F468-1F3FC-200D-1F9B1', '1F468-1F3FC-200D-1F9B2', '1F468-1F3FC-200D-1F9B3', '1F468-1F3FC-200D-1F9BC', '1F468-1F3FC-200D-1F9BD', '1F468-1F3FC-200D-2695-FE0F', '1F468-1F3FC-200D-2696-FE0F', '1F468-1F3FC-200D-2708-FE0F', '1F468-1F3FC', '1F468-1F3FD-200D-1F33E', '1F468-1F3FD-200D-1F373', '1F468-1F3FD-200D-1F37C', '1F468-1F3FD-200D-1F393', '1F468-1F3FD-200D-1F3A4', '1F468-1F3FD-200D-1F3A8', '1F468-1F3FD-200D-1F3EB', '1F468-1F3FD-200D-1F3ED', '1F468-1F3FD-200D-1F4BB', '1F468-1F3FD-200D-1F4BC', '1F468-1F3FD-200D-1F527', '1F468-1F3FD-200D-1F52C', '1F468-1F3FD-200D-1F680', '1F468-1F3FD-200D-1F692', '1F468-1F3FD-200D-1F91D-200D-1F468-1F3FB', '1F468-1F3FD-200D-1F91D-200D-1F468-1F3FC', '1F468-1F3FD-200D-1F91D-200D-1F468-1F3FE', '1F468-1F3FD-200D-1F91D-200D-1F468-1F3FF', '1F468-1F3FD-200D-1F9AF', '1F468-1F3FD-200D-1F9B0', '1F468-1F3FD-200D-1F9B1', '1F468-1F3FD-200D-1F9B2', '1F468-1F3FD-200D-1F9B3', '1F468-1F3FD-200D-1F9BC', '1F468-1F3FD-200D-1F9BD', '1F468-1F3FD-200D-2695-FE0F', '1F468-1F3FD-200D-2696-FE0F', '1F468-1F3FD-200D-2708-FE0F', '1F468-1F3FD', '1F468-1F3FE-200D-1F33E', '1F468-1F3FE-200D-1F373', '1F468-1F3FE-200D-1F37C', '1F468-1F3FE-200D-1F393', '1F468-1F3FE-200D-1F3A4', '1F468-1F3FE-200D-1F3A8', '1F468-1F3FE-200D-1F3EB', '1F468-1F3FE-200D-1F3ED', '1F468-1F3FE-200D-1F4BB', '1F468-1F3FE-200D-1F4BC', '1F468-1F3FE-200D-1F527', '1F468-1F3FE-200D-1F52C', '1F468-1F3FE-200D-1F680', '1F468-1F3FE-200D-1F692', '1F468-1F3FE-200D-1F91D-200D-1F468-1F3FB', '1F468-1F3FE-200D-1F91D-200D-1F468-1F3FC', '1F468-1F3FE-200D-1F91D-200D-1F468-1F3FD', '1F468-1F3FE-200D-1F91D-200D-1F468-1F3FF', '1F468-1F3FE-200D-1F9AF', '1F468-1F3FE-200D-1F9B0', '1F468-1F3FE-200D-1F9B1', '1F468-1F3FE-200D-1F9B2', '1F468-1F3FE-200D-1F9B3', '1F468-1F3FE-200D-1F9BC', '1F468-1F3FE-200D-1F9BD', '1F468-1F3FE-200D-2695-FE0F', '1F468-1F3FE-200D-2696-FE0F', '1F468-1F3FE-200D-2708-FE0F', '1F468-1F3FE', '1F468-1F3FF-200D-1F33E', '1F468-1F3FF-200D-1F373', '1F468-1F3FF-200D-1F37C', '1F468-1F3FF-200D-1F393', '1F468-1F3FF-200D-1F3A4', '1F468-1F3FF-200D-1F3A8', '1F468-1F3FF-200D-1F3EB', '1F468-1F3FF-200D-1F3ED', '1F468-1F3FF-200D-1F4BB', '1F468-1F3FF-200D-1F4BC', '1F468-1F3FF-200D-1F527', '1F468-1F3FF-200D-1F52C', '1F468-1F3FF-200D-1F680', '1F468-1F3FF-200D-1F692', '1F468-1F3FF-200D-1F91D-200D-1F468-1F3FB', '1F468-1F3FF-200D-1F91D-200D-1F468-1F3FC', '1F468-1F3FF-200D-1F91D-200D-1F468-1F3FD', '1F468-1F3FF-200D-1F91D-200D-1F468-1F3FE', '1F468-1F3FF-200D-1F9AF', '1F468-1F3FF-200D-1F9B0', '1F468-1F3FF-200D-1F9B1', '1F468-1F3FF-200D-1F9B2', '1F468-1F3FF-200D-1F9B3', '1F468-1F3FF-200D-1F9BC', '1F468-1F3FF-200D-1F9BD', '1F468-1F3FF-200D-2695-FE0F', '1F468-1F3FF-200D-2696-FE0F', '1F468-1F3FF-200D-2708-FE0F', '1F468-1F3FF', '1F468-200D-1F33E', '1F468-200D-1F373', '1F468-200D-1F37C', '1F468-200D-1F393', '1F468-200D-1F3A4', '1F468-200D-1F3A8', '1F468-200D-1F3EB', '1F468-200D-1F3ED', '1F468-200D-1F466-200D-1F466', '1F468-200D-1F466', '1F468-200D-1F467-200D-1F466', '1F468-200D-1F467-200D-1F467', '1F468-200D-1F467', '1F468-200D-1F468-200D-1F466-200D-1F466', '1F468-200D-1F468-200D-1F466', '1F468-200D-1F468-200D-1F467-200D-1F466', '1F468-200D-1F468-200D-1F467-200D-1F467', '1F468-200D-1F468-200D-1F467', '1F468-200D-1F469-200D-1F466-200D-1F466', '1F468-200D-1F469-200D-1F466', '1F468-200D-1F469-200D-1F467-200D-1F466', '1F468-200D-1F469-200D-1F467-200D-1F467', '1F468-200D-1F469-200D-1F467', '1F468-200D-1F4BB', '1F468-200D-1F4BC', '1F468-200D-1F527', '1F468-200D-1F52C', '1F468-200D-1F680', '1F468-200D-1F692', '1F468-200D-1F9AF', '1F468-200D-1F9B0', '1F468-200D-1F9B1', '1F468-200D-1F9B2', '1F468-200D-1F9B3', '1F468-200D-1F9BC', '1F468-200D-1F9BD', '1F468-200D-2695-FE0F', '1F468-200D-2696-FE0F', '1F468-200D-2708-FE0F', '1F468-200D-2764-FE0F-200D-1F468', '1F468-200D-2764-FE0F-200D-1F48B-200D-1F468', '1F468', '1F469-1F3FB-200D-1F33E', '1F469-1F3FB-200D-1F373', '1F469-1F3FB-200D-1F37C', '1F469-1F3FB-200D-1F393', '1F469-1F3FB-200D-1F3A4', '1F469-1F3FB-200D-1F3A8', '1F469-1F3FB-200D-1F3EB', '1F469-1F3FB-200D-1F3ED', '1F469-1F3FB-200D-1F4BB', '1F469-1F3FB-200D-1F4BC', '1F469-1F3FB-200D-1F527', '1F469-1F3FB-200D-1F52C', '1F469-1F3FB-200D-1F680', '1F469-1F3FB-200D-1F692', '1F469-1F3FB-200D-1F91D-200D-1F468-1F3FC', '1F469-1F3FB-200D-1F91D-200D-1F468-1F3FD', '1F469-1F3FB-200D-1F91D-200D-1F468-1F3FE', '1F469-1F3FB-200D-1F91D-200D-1F468-1F3FF', '1F469-1F3FB-200D-1F91D-200D-1F469-1F3FC', '1F469-1F3FB-200D-1F91D-200D-1F469-1F3FD', '1F469-1F3FB-200D-1F91D-200D-1F469-1F3FE', '1F469-1F3FB-200D-1F91D-200D-1F469-1F3FF', '1F469-1F3FB-200D-1F9AF', '1F469-1F3FB-200D-1F9B0', '1F469-1F3FB-200D-1F9B1', '1F469-1F3FB-200D-1F9B2', '1F469-1F3FB-200D-1F9B3', '1F469-1F3FB-200D-1F9BC', '1F469-1F3FB-200D-1F9BD', '1F469-1F3FB-200D-2695-FE0F', '1F469-1F3FB-200D-2696-FE0F', '1F469-1F3FB-200D-2708-FE0F', '1F469-1F3FB', '1F469-1F3FC-200D-1F33E', '1F469-1F3FC-200D-1F373', '1F469-1F3FC-200D-1F37C', '1F469-1F3FC-200D-1F393', '1F469-1F3FC-200D-1F3A4', '1F469-1F3FC-200D-1F3A8', '1F469-1F3FC-200D-1F3EB', '1F469-1F3FC-200D-1F3ED', '1F469-1F3FC-200D-1F4BB', '1F469-1F3FC-200D-1F4BC', '1F469-1F3FC-200D-1F527', '1F469-1F3FC-200D-1F52C', '1F469-1F3FC-200D-1F680', '1F469-1F3FC-200D-1F692', '1F469-1F3FC-200D-1F91D-200D-1F468-1F3FB', '1F469-1F3FC-200D-1F91D-200D-1F468-1F3FD', '1F469-1F3FC-200D-1F91D-200D-1F468-1F3FE', '1F469-1F3FC-200D-1F91D-200D-1F468-1F3FF', '1F469-1F3FC-200D-1F91D-200D-1F469-1F3FB', '1F469-1F3FC-200D-1F91D-200D-1F469-1F3FD', '1F469-1F3FC-200D-1F91D-200D-1F469-1F3FE', '1F469-1F3FC-200D-1F91D-200D-1F469-1F3FF', '1F469-1F3FC-200D-1F9AF', '1F469-1F3FC-200D-1F9B0', '1F469-1F3FC-200D-1F9B1', '1F469-1F3FC-200D-1F9B2', '1F469-1F3FC-200D-1F9B3', '1F469-1F3FC-200D-1F9BC', '1F469-1F3FC-200D-1F9BD', '1F469-1F3FC-200D-2695-FE0F', '1F469-1F3FC-200D-2696-FE0F', '1F469-1F3FC-200D-2708-FE0F', '1F469-1F3FC', '1F469-1F3FD-200D-1F33E', '1F469-1F3FD-200D-1F373', '1F469-1F3FD-200D-1F37C', '1F469-1F3FD-200D-1F393', '1F469-1F3FD-200D-1F3A4', '1F469-1F3FD-200D-1F3A8', '1F469-1F3FD-200D-1F3EB', '1F469-1F3FD-200D-1F3ED', '1F469-1F3FD-200D-1F4BB', '1F469-1F3FD-200D-1F4BC', '1F469-1F3FD-200D-1F527', '1F469-1F3FD-200D-1F52C', '1F469-1F3FD-200D-1F680', '1F469-1F3FD-200D-1F692', '1F469-1F3FD-200D-1F91D-200D-1F468-1F3FB', '1F469-1F3FD-200D-1F91D-200D-1F468-1F3FC', '1F469-1F3FD-200D-1F91D-200D-1F468-1F3FE', '1F469-1F3FD-200D-1F91D-200D-1F468-1F3FF', '1F469-1F3FD-200D-1F91D-200D-1F469-1F3FB', '1F469-1F3FD-200D-1F91D-200D-1F469-1F3FC', '1F469-1F3FD-200D-1F91D-200D-1F469-1F3FE', '1F469-1F3FD-200D-1F91D-200D-1F469-1F3FF', '1F469-1F3FD-200D-1F9AF', '1F469-1F3FD-200D-1F9B0', '1F469-1F3FD-200D-1F9B1', '1F469-1F3FD-200D-1F9B2', '1F469-1F3FD-200D-1F9B3', '1F469-1F3FD-200D-1F9BC', '1F469-1F3FD-200D-1F9BD', '1F469-1F3FD-200D-2695-FE0F', '1F469-1F3FD-200D-2696-FE0F', '1F469-1F3FD-200D-2708-FE0F', '1F469-1F3FD', '1F469-1F3FE-200D-1F33E', '1F469-1F3FE-200D-1F373', '1F469-1F3FE-200D-1F37C', '1F469-1F3FE-200D-1F393', '1F469-1F3FE-200D-1F3A4', '1F469-1F3FE-200D-1F3A8', '1F469-1F3FE-200D-1F3EB', '1F469-1F3FE-200D-1F3ED', '1F469-1F3FE-200D-1F4BB', '1F469-1F3FE-200D-1F4BC', '1F469-1F3FE-200D-1F527', '1F469-1F3FE-200D-1F52C', '1F469-1F3FE-200D-1F680', '1F469-1F3FE-200D-1F692', '1F469-1F3FE-200D-1F91D-200D-1F468-1F3FB', '1F469-1F3FE-200D-1F91D-200D-1F468-1F3FC', '1F469-1F3FE-200D-1F91D-200D-1F468-1F3FD', '1F469-1F3FE-200D-1F91D-200D-1F468-1F3FF', '1F469-1F3FE-200D-1F91D-200D-1F469-1F3FB', '1F469-1F3FE-200D-1F91D-200D-1F469-1F3FC', '1F469-1F3FE-200D-1F91D-200D-1F469-1F3FD', '1F469-1F3FE-200D-1F91D-200D-1F469-1F3FF', '1F469-1F3FE-200D-1F9AF', '1F469-1F3FE-200D-1F9B0', '1F469-1F3FE-200D-1F9B1', '1F469-1F3FE-200D-1F9B2', '1F469-1F3FE-200D-1F9B3', '1F469-1F3FE-200D-1F9BC', '1F469-1F3FE-200D-1F9BD', '1F469-1F3FE-200D-2695-FE0F', '1F469-1F3FE-200D-2696-FE0F', '1F469-1F3FE-200D-2708-FE0F', '1F469-1F3FE', '1F469-1F3FF-200D-1F33E', '1F469-1F3FF-200D-1F373', '1F469-1F3FF-200D-1F37C', '1F469-1F3FF-200D-1F393', '1F469-1F3FF-200D-1F3A4', '1F469-1F3FF-200D-1F3A8', '1F469-1F3FF-200D-1F3EB', '1F469-1F3FF-200D-1F3ED', '1F469-1F3FF-200D-1F4BB', '1F469-1F3FF-200D-1F4BC', '1F469-1F3FF-200D-1F527', '1F469-1F3FF-200D-1F52C', '1F469-1F3FF-200D-1F680', '1F469-1F3FF-200D-1F692', '1F469-1F3FF-200D-1F91D-200D-1F468-1F3FB', '1F469-1F3FF-200D-1F91D-200D-1F468-1F3FC', '1F469-1F3FF-200D-1F91D-200D-1F468-1F3FD', '1F469-1F3FF-200D-1F91D-200D-1F468-1F3FE', '1F469-1F3FF-200D-1F91D-200D-1F469-1F3FB', '1F469-1F3FF-200D-1F91D-200D-1F469-1F3FC', '1F469-1F3FF-200D-1F91D-200D-1F469-1F3FD', '1F469-1F3FF-200D-1F91D-200D-1F469-1F3FE', '1F469-1F3FF-200D-1F9AF', '1F469-1F3FF-200D-1F9B0', '1F469-1F3FF-200D-1F9B1', '1F469-1F3FF-200D-1F9B2', '1F469-1F3FF-200D-1F9B3', '1F469-1F3FF-200D-1F9BC', '1F469-1F3FF-200D-1F9BD', '1F469-1F3FF-200D-2695-FE0F', '1F469-1F3FF-200D-2696-FE0F', '1F469-1F3FF-200D-2708-FE0F', '1F469-1F3FF', '1F469-200D-1F33E', '1F469-200D-1F373', '1F469-200D-1F37C', '1F469-200D-1F393', '1F469-200D-1F3A4', '1F469-200D-1F3A8', '1F469-200D-1F3EB', '1F469-200D-1F3ED', '1F469-200D-1F466-200D-1F466', '1F469-200D-1F466', '1F469-200D-1F467-200D-1F466', '1F469-200D-1F467-200D-1F467', '1F469-200D-1F467', '1F469-200D-1F469-200D-1F466-200D-1F466', '1F469-200D-1F469-200D-1F466', '1F469-200D-1F469-200D-1F467-200D-1F466', '1F469-200D-1F469-200D-1F467-200D-1F467', '1F469-200D-1F469-200D-1F467', '1F469-200D-1F4BB', '1F469-200D-1F4BC', '1F469-200D-1F527', '1F469-200D-1F52C', '1F469-200D-1F680', '1F469-200D-1F692', '1F469-200D-1F9AF', '1F469-200D-1F9B0', '1F469-200D-1F9B1', '1F469-200D-1F9B2', '1F469-200D-1F9B3', '1F469-200D-1F9BC', '1F469-200D-1F9BD', '1F469-200D-2695-FE0F', '1F469-200D-2696-FE0F', '1F469-200D-2708-FE0F', '1F469-200D-2764-FE0F-200D-1F468', '1F469-200D-2764-FE0F-200D-1F469', '1F469-200D-2764-FE0F-200D-1F48B-200D-1F468', '1F469-200D-2764-FE0F-200D-1F48B-200D-1F469', '1F469', '1F46A', '1F46B-1F3FB', '1F46B-1F3FC', '1F46B-1F3FD', '1F46B-1F3FE', '1F46B-1F3FF', '1F46B', '1F46C-1F3FB', '1F46C-1F3FC', '1F46C-1F3FD', '1F46C-1F3FE', '1F46C-1F3FF', '1F46C', '1F46D-1F3FB', '1F46D-1F3FC', '1F46D-1F3FD', '1F46D-1F3FE', '1F46D-1F3FF', '1F46D', '1F46E-1F3FB-200D-2640-FE0F', '1F46E-1F3FB-200D-2642-FE0F', '1F46E-1F3FB', '1F46E-1F3FC-200D-2640-FE0F', '1F46E-1F3FC-200D-2642-FE0F', '1F46E-1F3FC', '1F46E-1F3FD-200D-2640-FE0F', '1F46E-1F3FD-200D-2642-FE0F', '1F46E-1F3FD', '1F46E-1F3FE-200D-2640-FE0F', '1F46E-1F3FE-200D-2642-FE0F', '1F46E-1F3FE', '1F46E-1F3FF-200D-2640-FE0F', '1F46E-1F3FF-200D-2642-FE0F', '1F46E-1F3FF', '1F46E-200D-2640-FE0F', '1F46E-200D-2642-FE0F', '1F46E', '1F46F-200D-2640-FE0F', '1F46F-200D-2642-FE0F', '1F46F', '1F470-1F3FB-200D-2640-FE0F', '1F470-1F3FB-200D-2642-FE0F', '1F470-1F3FB', '1F470-1F3FC-200D-2640-FE0F', '1F470-1F3FC-200D-2642-FE0F', '1F470-1F3FC', '1F470-1F3FD-200D-2640-FE0F', '1F470-1F3FD-200D-2642-FE0F', '1F470-1F3FD', '1F470-1F3FE-200D-2640-FE0F', '1F470-1F3FE-200D-2642-FE0F', '1F470-1F3FE', '1F470-1F3FF-200D-2640-FE0F', '1F470-1F3FF-200D-2642-FE0F', '1F470-1F3FF', '1F470-200D-2640-FE0F', '1F470-200D-2642-FE0F', '1F470', '1F471-1F3FB-200D-2640-FE0F', '1F471-1F3FB-200D-2642-FE0F', '1F471-1F3FB', '1F471-1F3FC-200D-2640-FE0F', '1F471-1F3FC-200D-2642-FE0F', '1F471-1F3FC', '1F471-1F3FD-200D-2640-FE0F', '1F471-1F3FD-200D-2642-FE0F', '1F471-1F3FD', '1F471-1F3FE-200D-2640-FE0F', '1F471-1F3FE-200D-2642-FE0F', '1F471-1F3FE', '1F471-1F3FF-200D-2640-FE0F', '1F471-1F3FF-200D-2642-FE0F', '1F471-1F3FF', '1F471-200D-2640-FE0F', '1F471-200D-2642-FE0F', '1F471', '1F472-1F3FB', '1F472-1F3FC', '1F472-1F3FD', '1F472-1F3FE', '1F472-1F3FF', '1F472', '1F473-1F3FB-200D-2640-FE0F', '1F473-1F3FB-200D-2642-FE0F', '1F473-1F3FB', '1F473-1F3FC-200D-2640-FE0F', '1F473-1F3FC-200D-2642-FE0F', '1F473-1F3FC', '1F473-1F3FD-200D-2640-FE0F', '1F473-1F3FD-200D-2642-FE0F', '1F473-1F3FD', '1F473-1F3FE-200D-2640-FE0F', '1F473-1F3FE-200D-2642-FE0F', '1F473-1F3FE', '1F473-1F3FF-200D-2640-FE0F', '1F473-1F3FF-200D-2642-FE0F', '1F473-1F3FF', '1F473-200D-2640-FE0F', '1F473-200D-2642-FE0F', '1F473', '1F474-1F3FB', '1F474-1F3FC', '1F474-1F3FD', '1F474-1F3FE', '1F474-1F3FF', '1F474', '1F475-1F3FB', '1F475-1F3FC', '1F475-1F3FD', '1F475-1F3FE', '1F475-1F3FF', '1F475', '1F476-1F3FB', '1F476-1F3FC', '1F476-1F3FD', '1F476-1F3FE', '1F476-1F3FF', '1F476', '1F477-1F3FB-200D-2640-FE0F', '1F477-1F3FB-200D-2642-FE0F', '1F477-1F3FB', '1F477-1F3FC-200D-2640-FE0F', '1F477-1F3FC-200D-2642-FE0F', '1F477-1F3FC', '1F477-1F3FD-200D-2640-FE0F', '1F477-1F3FD-200D-2642-FE0F', '1F477-1F3FD', '1F477-1F3FE-200D-2640-FE0F', '1F477-1F3FE-200D-2642-FE0F', '1F477-1F3FE', '1F477-1F3FF-200D-2640-FE0F', '1F477-1F3FF-200D-2642-FE0F', '1F477-1F3FF', '1F477-200D-2640-FE0F', '1F477-200D-2642-FE0F', '1F477', '1F478-1F3FB', '1F478-1F3FC', '1F478-1F3FD', '1F478-1F3FE', '1F478-1F3FF', '1F478', '1F479', '1F47A', '1F47B', '1F47C-1F3FB', '1F47C-1F3FC', '1F47C-1F3FD', '1F47C-1F3FE', '1F47C-1F3FF', '1F47C', '1F47D', '1F47E', '1F47F', '1F480', '1F481-1F3FB-200D-2640-FE0F', '1F481-1F3FB-200D-2642-FE0F', '1F481-1F3FB', '1F481-1F3FC-200D-2640-FE0F', '1F481-1F3FC-200D-2642-FE0F', '1F481-1F3FC', '1F481-1F3FD-200D-2640-FE0F', '1F481-1F3FD-200D-2642-FE0F', '1F481-1F3FD', '1F481-1F3FE-200D-2640-FE0F', '1F481-1F3FE-200D-2642-FE0F', '1F481-1F3FE', '1F481-1F3FF-200D-2640-FE0F', '1F481-1F3FF-200D-2642-FE0F', '1F481-1F3FF', '1F481-200D-2640-FE0F', '1F481-200D-2642-FE0F', '1F481', '1F482-1F3FB-200D-2640-FE0F', '1F482-1F3FB-200D-2642-FE0F', '1F482-1F3FB', '1F482-1F3FC-200D-2640-FE0F', '1F482-1F3FC-200D-2642-FE0F', '1F482-1F3FC', '1F482-1F3FD-200D-2640-FE0F', '1F482-1F3FD-200D-2642-FE0F', '1F482-1F3FD', '1F482-1F3FE-200D-2640-FE0F', '1F482-1F3FE-200D-2642-FE0F', '1F482-1F3FE', '1F482-1F3FF-200D-2640-FE0F', '1F482-1F3FF-200D-2642-FE0F', '1F482-1F3FF', '1F482-200D-2640-FE0F', '1F482-200D-2642-FE0F', '1F482', '1F483-1F3FB', '1F483-1F3FC', '1F483-1F3FD', '1F483-1F3FE', '1F483-1F3FF', '1F483', '1F484', '1F485-1F3FB', '1F485-1F3FC', '1F485-1F3FD', '1F485-1F3FE', '1F485-1F3FF', '1F485', '1F486-1F3FB-200D-2640-FE0F', '1F486-1F3FB-200D-2642-FE0F', '1F486-1F3FB', '1F486-1F3FC-200D-2640-FE0F', '1F486-1F3FC-200D-2642-FE0F', '1F486-1F3FC', '1F486-1F3FD-200D-2640-FE0F', '1F486-1F3FD-200D-2642-FE0F', '1F486-1F3FD', '1F486-1F3FE-200D-2640-FE0F', '1F486-1F3FE-200D-2642-FE0F', '1F486-1F3FE', '1F486-1F3FF-200D-2640-FE0F', '1F486-1F3FF-200D-2642-FE0F', '1F486-1F3FF', '1F486-200D-2640-FE0F', '1F486-200D-2642-FE0F', '1F486', '1F487-1F3FB-200D-2640-FE0F', '1F487-1F3FB-200D-2642-FE0F', '1F487-1F3FB', '1F487-1F3FC-200D-2640-FE0F', '1F487-1F3FC-200D-2642-FE0F', '1F487-1F3FC', '1F487-1F3FD-200D-2640-FE0F', '1F487-1F3FD-200D-2642-FE0F', '1F487-1F3FD', '1F487-1F3FE-200D-2640-FE0F', '1F487-1F3FE-200D-2642-FE0F', '1F487-1F3FE', '1F487-1F3FF-200D-2640-FE0F', '1F487-1F3FF-200D-2642-FE0F', '1F487-1F3FF', '1F487-200D-2640-FE0F', '1F487-200D-2642-FE0F', '1F487', '1F488', '1F489', '1F48A', '1F48B', '1F48C', '1F48D', '1F48E', '1F48F', '1F490', '1F491', '1F492', '1F493', '1F494', '1F495', '1F496', '1F497', '1F498', '1F499', '1F49A', '1F49B', '1F49C', '1F49D', '1F49E', '1F49F', '1F4A0', '1F4A1', '1F4A2', '1F4A3', '1F4A4', '1F4A5', '1F4A6', '1F4A7', '1F4A8', '1F4A9', '1F4AA-1F3FB', '1F4AA-1F3FC', '1F4AA-1F3FD', '1F4AA-1F3FE', '1F4AA-1F3FF', '1F4AA', '1F4AB', '1F4AC', '1F4AD', '1F4AE', '1F4AF', '1F4B0', '1F4B1', '1F4B2', '1F4B3', '1F4B4', '1F4B5', '1F4B6', '1F4B7', '1F4B8', '1F4B9', '1F4BA', '1F4BB', '1F4BC', '1F4BD', '1F4BE', '1F4BF', '1F4C0', '1F4C1', '1F4C2', '1F4C3', '1F4C4', '1F4C5', '1F4C6', '1F4C7', '1F4C8', '1F4C9', '1F4CA', '1F4CB', '1F4CC', '1F4CD', '1F4CE', '1F4CF', '1F4D0', '1F4D1', '1F4D2', '1F4D3', '1F4D4', '1F4D5', '1F4D6', '1F4D7', '1F4D8', '1F4D9', '1F4DA', '1F4DB', '1F4DC', '1F4DD', '1F4DE', '1F4DF', '1F4E0', '1F4E1', '1F4E2', '1F4E3', '1F4E4', '1F4E5', '1F4E6', '1F4E7', '1F4E8', '1F4E9', '1F4EA', '1F4EB', '1F4EC', '1F4ED', '1F4EE', '1F4EF', '1F4F0', '1F4F1', '1F4F2', '1F4F3', '1F4F4', '1F4F5', '1F4F6', '1F4F7', '1F4F8', '1F4F9', '1F4FA', '1F4FB', '1F4FC', '1F4FD', '1F4FF', '1F500', '1F501', '1F502', '1F503', '1F504', '1F505', '1F506', '1F507', '1F508', '1F509', '1F50A', '1F50B', '1F50C', '1F50D', '1F50E', '1F50F', '1F510', '1F511', '1F512', '1F513', '1F514', '1F515', '1F516', '1F517', '1F518', '1F519', '1F51A', '1F51B', '1F51C', '1F51D', '1F51E', '1F51F', '1F520', '1F521', '1F522', '1F523', '1F524', '1F525', '1F526', '1F527', '1F528', '1F529', '1F52A', '1F52B', '1F52C', '1F52D', '1F52E', '1F52F', '1F530', '1F531', '1F532', '1F533', '1F534', '1F535', '1F536', '1F537', '1F538', '1F539', '1F53A', '1F53B', '1F53C', '1F53D', '1F549', '1F54A', '1F54B', '1F54C', '1F54D', '1F54E', '1F550', '1F551', '1F552', '1F553', '1F554', '1F555', '1F556', '1F557', '1F558', '1F559', '1F55A', '1F55B', '1F55C', '1F55D', '1F55E', '1F55F', '1F560', '1F561', '1F562', '1F563', '1F564', '1F565', '1F566', '1F567', '1F56F', '1F570', '1F573', '1F574-1F3FB', '1F574-1F3FC', '1F574-1F3FD', '1F574-1F3FE', '1F574-1F3FF', '1F574', '1F575-1F3FB-200D-2640-FE0F', '1F575-1F3FB-200D-2642-FE0F', '1F575-1F3FB', '1F575-1F3FC-200D-2640-FE0F', '1F575-1F3FC-200D-2642-FE0F', '1F575-1F3FC', '1F575-1F3FD-200D-2640-FE0F', '1F575-1F3FD-200D-2642-FE0F', '1F575-1F3FD', '1F575-1F3FE-200D-2640-FE0F', '1F575-1F3FE-200D-2642-FE0F', '1F575-1F3FE', '1F575-1F3FF-200D-2640-FE0F', '1F575-1F3FF-200D-2642-FE0F', '1F575-1F3FF', '1F575-FE0F-200D-2640-FE0F', '1F575-FE0F-200D-2642-FE0F', '1F575', '1F576', '1F577', '1F578', '1F579', '1F57A-1F3FB', '1F57A-1F3FC', '1F57A-1F3FD', '1F57A-1F3FE', '1F57A-1F3FF', '1F57A', '1F587', '1F58A', '1F58B', '1F58C', '1F58D', '1F590-1F3FB', '1F590-1F3FC', '1F590-1F3FD', '1F590-1F3FE', '1F590-1F3FF', '1F590', '1F595-1F3FB', '1F595-1F3FC', '1F595-1F3FD', '1F595-1F3FE', '1F595-1F3FF', '1F595', '1F596-1F3FB', '1F596-1F3FC', '1F596-1F3FD', '1F596-1F3FE', '1F596-1F3FF', '1F596', '1F5A4', '1F5A5', '1F5A8', '1F5B1', '1F5B2', '1F5BC', '1F5C2', '1F5C3', '1F5C4', '1F5D1', '1F5D2', '1F5D3', '1F5DC', '1F5DD', '1F5DE', '1F5E1', '1F5E3', '1F5E8', '1F5EF', '1F5F3', '1F5FA', '1F5FB', '1F5FC', '1F5FD', '1F5FE', '1F5FF', '1F600', '1F601', '1F602', '1F603', '1F604', '1F605', '1F606', '1F607', '1F608', '1F609', '1F60A', '1F60B', '1F60C', '1F60D', '1F60E', '1F60F', '1F610', '1F611', '1F612', '1F613', '1F614', '1F615', '1F616', '1F617', '1F618', '1F619', '1F61A', '1F61B', '1F61C', '1F61D', '1F61E', '1F61F', '1F620', '1F621', '1F622', '1F623', '1F624', '1F625', '1F626', '1F627', '1F628', '1F629', '1F62A', '1F62B', '1F62C', '1F62D', '1F62E', '1F62F', '1F630', '1F631', '1F632', '1F633', '1F634', '1F635', '1F636', '1F637', '1F638', '1F639', '1F63A', '1F63B', '1F63C', '1F63D', '1F63E', '1F63F', '1F640', '1F641', '1F642', '1F643', '1F644', '1F645-1F3FB-200D-2640-FE0F', '1F645-1F3FB-200D-2642-FE0F', '1F645-1F3FB', '1F645-1F3FC-200D-2640-FE0F', '1F645-1F3FC-200D-2642-FE0F', '1F645-1F3FC', '1F645-1F3FD-200D-2640-FE0F', '1F645-1F3FD-200D-2642-FE0F', '1F645-1F3FD', '1F645-1F3FE-200D-2640-FE0F', '1F645-1F3FE-200D-2642-FE0F', '1F645-1F3FE', '1F645-1F3FF-200D-2640-FE0F', '1F645-1F3FF-200D-2642-FE0F', '1F645-1F3FF', '1F645-200D-2640-FE0F', '1F645-200D-2642-FE0F', '1F645', '1F646-1F3FB-200D-2640-FE0F', '1F646-1F3FB-200D-2642-FE0F', '1F646-1F3FB', '1F646-1F3FC-200D-2640-FE0F', '1F646-1F3FC-200D-2642-FE0F', '1F646-1F3FC', '1F646-1F3FD-200D-2640-FE0F', '1F646-1F3FD-200D-2642-FE0F', '1F646-1F3FD', '1F646-1F3FE-200D-2640-FE0F', '1F646-1F3FE-200D-2642-FE0F', '1F646-1F3FE', '1F646-1F3FF-200D-2640-FE0F', '1F646-1F3FF-200D-2642-FE0F', '1F646-1F3FF', '1F646-200D-2640-FE0F', '1F646-200D-2642-FE0F', '1F646', '1F647-1F3FB-200D-2640-FE0F', '1F647-1F3FB-200D-2642-FE0F', '1F647-1F3FB', '1F647-1F3FC-200D-2640-FE0F', '1F647-1F3FC-200D-2642-FE0F', '1F647-1F3FC', '1F647-1F3FD-200D-2640-FE0F', '1F647-1F3FD-200D-2642-FE0F', '1F647-1F3FD', '1F647-1F3FE-200D-2640-FE0F', '1F647-1F3FE-200D-2642-FE0F', '1F647-1F3FE', '1F647-1F3FF-200D-2640-FE0F', '1F647-1F3FF-200D-2642-FE0F', '1F647-1F3FF', '1F647-200D-2640-FE0F', '1F647-200D-2642-FE0F', '1F647', '1F648', '1F649', '1F64A', '1F64B-1F3FB-200D-2640-FE0F', '1F64B-1F3FB-200D-2642-FE0F', '1F64B-1F3FB', '1F64B-1F3FC-200D-2640-FE0F', '1F64B-1F3FC-200D-2642-FE0F', '1F64B-1F3FC', '1F64B-1F3FD-200D-2640-FE0F', '1F64B-1F3FD-200D-2642-FE0F', '1F64B-1F3FD', '1F64B-1F3FE-200D-2640-FE0F', '1F64B-1F3FE-200D-2642-FE0F', '1F64B-1F3FE', '1F64B-1F3FF-200D-2640-FE0F', '1F64B-1F3FF-200D-2642-FE0F', '1F64B-1F3FF', '1F64B-200D-2640-FE0F', '1F64B-200D-2642-FE0F', '1F64B', '1F64C-1F3FB', '1F64C-1F3FC', '1F64C-1F3FD', '1F64C-1F3FE', '1F64C-1F3FF', '1F64C', '1F64D-1F3FB-200D-2640-FE0F', '1F64D-1F3FB-200D-2642-FE0F', '1F64D-1F3FB', '1F64D-1F3FC-200D-2640-FE0F', '1F64D-1F3FC-200D-2642-FE0F', '1F64D-1F3FC', '1F64D-1F3FD-200D-2640-FE0F', '1F64D-1F3FD-200D-2642-FE0F', '1F64D-1F3FD', '1F64D-1F3FE-200D-2640-FE0F', '1F64D-1F3FE-200D-2642-FE0F', '1F64D-1F3FE', '1F64D-1F3FF-200D-2640-FE0F', '1F64D-1F3FF-200D-2642-FE0F', '1F64D-1F3FF', '1F64D-200D-2640-FE0F', '1F64D-200D-2642-FE0F', '1F64D', '1F64E-1F3FB-200D-2640-FE0F', '1F64E-1F3FB-200D-2642-FE0F', '1F64E-1F3FB', '1F64E-1F3FC-200D-2640-FE0F', '1F64E-1F3FC-200D-2642-FE0F', '1F64E-1F3FC', '1F64E-1F3FD-200D-2640-FE0F', '1F64E-1F3FD-200D-2642-FE0F', '1F64E-1F3FD', '1F64E-1F3FE-200D-2640-FE0F', '1F64E-1F3FE-200D-2642-FE0F', '1F64E-1F3FE', '1F64E-1F3FF-200D-2640-FE0F', '1F64E-1F3FF-200D-2642-FE0F', '1F64E-1F3FF', '1F64E-200D-2640-FE0F', '1F64E-200D-2642-FE0F', '1F64E', '1F64F-1F3FB', '1F64F-1F3FC', '1F64F-1F3FD', '1F64F-1F3FE', '1F64F-1F3FF', '1F64F', '1F680', '1F681', '1F682', '1F683', '1F684', '1F685', '1F686', '1F687', '1F688', '1F689', '1F68A', '1F68B', '1F68C', '1F68D', '1F68E', '1F68F', '1F690', '1F691', '1F692', '1F693', '1F694', '1F695', '1F696', '1F697', '1F698', '1F699', '1F69A', '1F69B', '1F69C', '1F69D', '1F69E', '1F69F', '1F6A0', '1F6A1', '1F6A2', '1F6A3-1F3FB-200D-2640-FE0F', '1F6A3-1F3FB-200D-2642-FE0F', '1F6A3-1F3FB', '1F6A3-1F3FC-200D-2640-FE0F', '1F6A3-1F3FC-200D-2642-FE0F', '1F6A3-1F3FC', '1F6A3-1F3FD-200D-2640-FE0F', '1F6A3-1F3FD-200D-2642-FE0F', '1F6A3-1F3FD', '1F6A3-1F3FE-200D-2640-FE0F', '1F6A3-1F3FE-200D-2642-FE0F', '1F6A3-1F3FE', '1F6A3-1F3FF-200D-2640-FE0F', '1F6A3-1F3FF-200D-2642-FE0F', '1F6A3-1F3FF', '1F6A3-200D-2640-FE0F', '1F6A3-200D-2642-FE0F', '1F6A3', '1F6A4', '1F6A5', '1F6A6', '1F6A7', '1F6A8', '1F6A9', '1F6AA', '1F6AB', '1F6AC', '1F6AD', '1F6AE', '1F6AF', '1F6B0', '1F6B1', '1F6B2', '1F6B3', '1F6B4-1F3FB-200D-2640-FE0F', '1F6B4-1F3FB-200D-2642-FE0F', '1F6B4-1F3FB', '1F6B4-1F3FC-200D-2640-FE0F', '1F6B4-1F3FC-200D-2642-FE0F', '1F6B4-1F3FC', '1F6B4-1F3FD-200D-2640-FE0F', '1F6B4-1F3FD-200D-2642-FE0F', '1F6B4-1F3FD', '1F6B4-1F3FE-200D-2640-FE0F', '1F6B4-1F3FE-200D-2642-FE0F', '1F6B4-1F3FE', '1F6B4-1F3FF-200D-2640-FE0F', '1F6B4-1F3FF-200D-2642-FE0F', '1F6B4-1F3FF', '1F6B4-200D-2640-FE0F', '1F6B4-200D-2642-FE0F', '1F6B4', '1F6B5-1F3FB-200D-2640-FE0F', '1F6B5-1F3FB-200D-2642-FE0F', '1F6B5-1F3FB', '1F6B5-1F3FC-200D-2640-FE0F', '1F6B5-1F3FC-200D-2642-FE0F', '1F6B5-1F3FC', '1F6B5-1F3FD-200D-2640-FE0F', '1F6B5-1F3FD-200D-2642-FE0F', '1F6B5-1F3FD', '1F6B5-1F3FE-200D-2640-FE0F', '1F6B5-1F3FE-200D-2642-FE0F', '1F6B5-1F3FE', '1F6B5-1F3FF-200D-2640-FE0F', '1F6B5-1F3FF-200D-2642-FE0F', '1F6B5-1F3FF', '1F6B5-200D-2640-FE0F', '1F6B5-200D-2642-FE0F', '1F6B5', '1F6B6-1F3FB-200D-2640-FE0F', '1F6B6-1F3FB-200D-2642-FE0F', '1F6B6-1F3FB', '1F6B6-1F3FC-200D-2640-FE0F', '1F6B6-1F3FC-200D-2642-FE0F', '1F6B6-1F3FC', '1F6B6-1F3FD-200D-2640-FE0F', '1F6B6-1F3FD-200D-2642-FE0F', '1F6B6-1F3FD', '1F6B6-1F3FE-200D-2640-FE0F', '1F6B6-1F3FE-200D-2642-FE0F', '1F6B6-1F3FE', '1F6B6-1F3FF-200D-2640-FE0F', '1F6B6-1F3FF-200D-2642-FE0F', '1F6B6-1F3FF', '1F6B6-200D-2640-FE0F', '1F6B6-200D-2642-FE0F', '1F6B6', '1F6B7', '1F6B8', '1F6B9', '1F6BA', '1F6BB', '1F6BC', '1F6BD', '1F6BE', '1F6BF', '1F6C0-1F3FB', '1F6C0-1F3FC', '1F6C0-1F3FD', '1F6C0-1F3FE', '1F6C0-1F3FF', '1F6C0', '1F6C1', '1F6C2', '1F6C3', '1F6C4', '1F6C5', '1F6CB', '1F6CC-1F3FB', '1F6CC-1F3FC', '1F6CC-1F3FD', '1F6CC-1F3FE', '1F6CC-1F3FF', '1F6CC', '1F6CD', '1F6CE', '1F6CF', '1F6D0', '1F6D1', '1F6D2', '1F6D5', '1F6D6', '1F6D7', '1F6E0', '1F6E1', '1F6E2', '1F6E3', '1F6E4', '1F6E5', '1F6E9', '1F6EB', '1F6EC', '1F6F0', '1F6F3', '1F6F4', '1F6F5', '1F6F6', '1F6F7', '1F6F8', '1F6F9', '1F6FA', '1F6FB', '1F6FC', '1F7E0', '1F7E1', '1F7E2', '1F7E3', '1F7E4', '1F7E5', '1F7E6', '1F7E7', '1F7E8', '1F7E9', '1F7EA', '1F7EB', '1F90C-1F3FB', '1F90C-1F3FC', '1F90C-1F3FD', '1F90C-1F3FE', '1F90C-1F3FF', '1F90C', '1F90D', '1F90E', '1F90F-1F3FB', '1F90F-1F3FC', '1F90F-1F3FD', '1F90F-1F3FE', '1F90F-1F3FF', '1F90F', '1F910', '1F911', '1F912', '1F913', '1F914', '1F915', '1F916', '1F917', '1F918-1F3FB', '1F918-1F3FC', '1F918-1F3FD', '1F918-1F3FE', '1F918-1F3FF', '1F918', '1F919-1F3FB', '1F919-1F3FC', '1F919-1F3FD', '1F919-1F3FE', '1F919-1F3FF', '1F919', '1F91A-1F3FB', '1F91A-1F3FC', '1F91A-1F3FD', '1F91A-1F3FE', '1F91A-1F3FF', '1F91A', '1F91B-1F3FB', '1F91B-1F3FC', '1F91B-1F3FD', '1F91B-1F3FE', '1F91B-1F3FF', '1F91B', '1F91C-1F3FB', '1F91C-1F3FC', '1F91C-1F3FD', '1F91C-1F3FE', '1F91C-1F3FF', '1F91C', '1F91D', '1F91E-1F3FB', '1F91E-1F3FC', '1F91E-1F3FD', '1F91E-1F3FE', '1F91E-1F3FF', '1F91E', '1F91F-1F3FB', '1F91F-1F3FC', '1F91F-1F3FD', '1F91F-1F3FE', '1F91F-1F3FF', '1F91F', '1F920', '1F921', '1F922', '1F923', '1F924', '1F925', '1F926-1F3FB-200D-2640-FE0F', '1F926-1F3FB-200D-2642-FE0F', '1F926-1F3FB', '1F926-1F3FC-200D-2640-FE0F', '1F926-1F3FC-200D-2642-FE0F', '1F926-1F3FC', '1F926-1F3FD-200D-2640-FE0F', '1F926-1F3FD-200D-2642-FE0F', '1F926-1F3FD', '1F926-1F3FE-200D-2640-FE0F', '1F926-1F3FE-200D-2642-FE0F', '1F926-1F3FE', '1F926-1F3FF-200D-2640-FE0F', '1F926-1F3FF-200D-2642-FE0F', '1F926-1F3FF', '1F926-200D-2640-FE0F', '1F926-200D-2642-FE0F', '1F926', '1F927', '1F928', '1F929', '1F92A', '1F92B', '1F92C', '1F92D', '1F92E', '1F92F', '1F930-1F3FB', '1F930-1F3FC', '1F930-1F3FD', '1F930-1F3FE', '1F930-1F3FF', '1F930', '1F931-1F3FB', '1F931-1F3FC', '1F931-1F3FD', '1F931-1F3FE', '1F931-1F3FF', '1F931', '1F932-1F3FB', '1F932-1F3FC', '1F932-1F3FD', '1F932-1F3FE', '1F932-1F3FF', '1F932', '1F933-1F3FB', '1F933-1F3FC', '1F933-1F3FD', '1F933-1F3FE', '1F933-1F3FF', '1F933', '1F934-1F3FB', '1F934-1F3FC', '1F934-1F3FD', '1F934-1F3FE', '1F934-1F3FF', '1F934', '1F935-1F3FB-200D-2640-FE0F', '1F935-1F3FB-200D-2642-FE0F', '1F935-1F3FB', '1F935-1F3FC-200D-2640-FE0F', '1F935-1F3FC-200D-2642-FE0F', '1F935-1F3FC', '1F935-1F3FD-200D-2640-FE0F', '1F935-1F3FD-200D-2642-FE0F', '1F935-1F3FD', '1F935-1F3FE-200D-2640-FE0F', '1F935-1F3FE-200D-2642-FE0F', '1F935-1F3FE', '1F935-1F3FF-200D-2640-FE0F', '1F935-1F3FF-200D-2642-FE0F', '1F935-1F3FF', '1F935-200D-2640-FE0F', '1F935-200D-2642-FE0F', '1F935', '1F936-1F3FB', '1F936-1F3FC', '1F936-1F3FD', '1F936-1F3FE', '1F936-1F3FF', '1F936', '1F937-1F3FB-200D-2640-FE0F', '1F937-1F3FB-200D-2642-FE0F', '1F937-1F3FB', '1F937-1F3FC-200D-2640-FE0F', '1F937-1F3FC-200D-2642-FE0F', '1F937-1F3FC', '1F937-1F3FD-200D-2640-FE0F', '1F937-1F3FD-200D-2642-FE0F', '1F937-1F3FD', '1F937-1F3FE-200D-2640-FE0F', '1F937-1F3FE-200D-2642-FE0F', '1F937-1F3FE', '1F937-1F3FF-200D-2640-FE0F', '1F937-1F3FF-200D-2642-FE0F', '1F937-1F3FF', '1F937-200D-2640-FE0F', '1F937-200D-2642-FE0F', '1F937', '1F938-1F3FB-200D-2640-FE0F', '1F938-1F3FB-200D-2642-FE0F', '1F938-1F3FB', '1F938-1F3FC-200D-2640-FE0F', '1F938-1F3FC-200D-2642-FE0F', '1F938-1F3FC', '1F938-1F3FD-200D-2640-FE0F', '1F938-1F3FD-200D-2642-FE0F', '1F938-1F3FD', '1F938-1F3FE-200D-2640-FE0F', '1F938-1F3FE-200D-2642-FE0F', '1F938-1F3FE', '1F938-1F3FF-200D-2640-FE0F', '1F938-1F3FF-200D-2642-FE0F', '1F938-1F3FF', '1F938-200D-2640-FE0F', '1F938-200D-2642-FE0F', '1F938', '1F939-1F3FB-200D-2640-FE0F', '1F939-1F3FB-200D-2642-FE0F', '1F939-1F3FB', '1F939-1F3FC-200D-2640-FE0F', '1F939-1F3FC-200D-2642-FE0F', '1F939-1F3FC', '1F939-1F3FD-200D-2640-FE0F', '1F939-1F3FD-200D-2642-FE0F', '1F939-1F3FD', '1F939-1F3FE-200D-2640-FE0F', '1F939-1F3FE-200D-2642-FE0F', '1F939-1F3FE', '1F939-1F3FF-200D-2640-FE0F', '1F939-1F3FF-200D-2642-FE0F', '1F939-1F3FF', '1F939-200D-2640-FE0F', '1F939-200D-2642-FE0F', '1F939', '1F93A', '1F93C-200D-2640-FE0F', '1F93C-200D-2642-FE0F', '1F93C', '1F93D-1F3FB-200D-2640-FE0F', '1F93D-1F3FB-200D-2642-FE0F', '1F93D-1F3FB', '1F93D-1F3FC-200D-2640-FE0F', '1F93D-1F3FC-200D-2642-FE0F', '1F93D-1F3FC', '1F93D-1F3FD-200D-2640-FE0F', '1F93D-1F3FD-200D-2642-FE0F', '1F93D-1F3FD', '1F93D-1F3FE-200D-2640-FE0F', '1F93D-1F3FE-200D-2642-FE0F', '1F93D-1F3FE', '1F93D-1F3FF-200D-2640-FE0F', '1F93D-1F3FF-200D-2642-FE0F', '1F93D-1F3FF', '1F93D-200D-2640-FE0F', '1F93D-200D-2642-FE0F', '1F93D', '1F93E-1F3FB-200D-2640-FE0F', '1F93E-1F3FB-200D-2642-FE0F', '1F93E-1F3FB', '1F93E-1F3FC-200D-2640-FE0F', '1F93E-1F3FC-200D-2642-FE0F', '1F93E-1F3FC', '1F93E-1F3FD-200D-2640-FE0F', '1F93E-1F3FD-200D-2642-FE0F', '1F93E-1F3FD', '1F93E-1F3FE-200D-2640-FE0F', '1F93E-1F3FE-200D-2642-FE0F', '1F93E-1F3FE', '1F93E-1F3FF-200D-2640-FE0F', '1F93E-1F3FF-200D-2642-FE0F', '1F93E-1F3FF', '1F93E-200D-2640-FE0F', '1F93E-200D-2642-FE0F', '1F93E', '1F93F', '1F940', '1F941', '1F942', '1F943', '1F944', '1F945', '1F947', '1F948', '1F949', '1F94A', '1F94B', '1F94C', '1F94D', '1F94E', '1F94F', '1F950', '1F951', '1F952', '1F953', '1F954', '1F955', '1F956', '1F957', '1F958', '1F959', '1F95A', '1F95B', '1F95C', '1F95D', '1F95E', '1F95F', '1F960', '1F961', '1F962', '1F963', '1F964', '1F965', '1F966', '1F967', '1F968', '1F969', '1F96A', '1F96B', '1F96C', '1F96D', '1F96E', '1F96F', '1F970', '1F971', '1F972', '1F973', '1F974', '1F975', '1F976', '1F977-1F3FB', '1F977-1F3FC', '1F977-1F3FD', '1F977-1F3FE', '1F977-1F3FF', '1F977', '1F978', '1F97A', '1F97B', '1F97C', '1F97D', '1F97E', '1F97F', '1F980', '1F981', '1F982', '1F983', '1F984', '1F985', '1F986', '1F987', '1F988', '1F989', '1F98A', '1F98B', '1F98C', '1F98D', '1F98E', '1F98F', '1F990', '1F991', '1F992', '1F993', '1F994', '1F995', '1F996', '1F997', '1F998', '1F999', '1F99A', '1F99B', '1F99C', '1F99D', '1F99E', '1F99F', '1F9A0', '1F9A1', '1F9A2', '1F9A3', '1F9A4', '1F9A5', '1F9A6', '1F9A7', '1F9A8', '1F9A9', '1F9AA', '1F9AB', '1F9AC', '1F9AD', '1F9AE', '1F9AF', '1F9B0', '1F9B1', '1F9B2', '1F9B3', '1F9B4', '1F9B5-1F3FB', '1F9B5-1F3FC', '1F9B5-1F3FD', '1F9B5-1F3FE', '1F9B5-1F3FF', '1F9B5', '1F9B6-1F3FB', '1F9B6-1F3FC', '1F9B6-1F3FD', '1F9B6-1F3FE', '1F9B6-1F3FF', '1F9B6', '1F9B7', '1F9B8-1F3FB-200D-2640-FE0F', '1F9B8-1F3FB-200D-2642-FE0F', '1F9B8-1F3FB', '1F9B8-1F3FC-200D-2640-FE0F', '1F9B8-1F3FC-200D-2642-FE0F', '1F9B8-1F3FC', '1F9B8-1F3FD-200D-2640-FE0F', '1F9B8-1F3FD-200D-2642-FE0F', '1F9B8-1F3FD', '1F9B8-1F3FE-200D-2640-FE0F', '1F9B8-1F3FE-200D-2642-FE0F', '1F9B8-1F3FE', '1F9B8-1F3FF-200D-2640-FE0F', '1F9B8-1F3FF-200D-2642-FE0F', '1F9B8-1F3FF', '1F9B8-200D-2640-FE0F', '1F9B8-200D-2642-FE0F', '1F9B8', '1F9B9-1F3FB-200D-2640-FE0F', '1F9B9-1F3FB-200D-2642-FE0F', '1F9B9-1F3FB', '1F9B9-1F3FC-200D-2640-FE0F', '1F9B9-1F3FC-200D-2642-FE0F', '1F9B9-1F3FC', '1F9B9-1F3FD-200D-2640-FE0F', '1F9B9-1F3FD-200D-2642-FE0F', '1F9B9-1F3FD', '1F9B9-1F3FE-200D-2640-FE0F', '1F9B9-1F3FE-200D-2642-FE0F', '1F9B9-1F3FE', '1F9B9-1F3FF-200D-2640-FE0F', '1F9B9-1F3FF-200D-2642-FE0F', '1F9B9-1F3FF', '1F9B9-200D-2640-FE0F', '1F9B9-200D-2642-FE0F', '1F9B9', '1F9BA', '1F9BB-1F3FB', '1F9BB-1F3FC', '1F9BB-1F3FD', '1F9BB-1F3FE', '1F9BB-1F3FF', '1F9BB', '1F9BC', '1F9BD', '1F9BE', '1F9BF', '1F9C0', '1F9C1', '1F9C2', '1F9C3', '1F9C4', '1F9C5', '1F9C6', '1F9C7', '1F9C8', '1F9C9', '1F9CA', '1F9CB', '1F9CD-1F3FB-200D-2640-FE0F', '1F9CD-1F3FB-200D-2642-FE0F', '1F9CD-1F3FB', '1F9CD-1F3FC-200D-2640-FE0F', '1F9CD-1F3FC-200D-2642-FE0F', '1F9CD-1F3FC', '1F9CD-1F3FD-200D-2640-FE0F', '1F9CD-1F3FD-200D-2642-FE0F', '1F9CD-1F3FD', '1F9CD-1F3FE-200D-2640-FE0F', '1F9CD-1F3FE-200D-2642-FE0F', '1F9CD-1F3FE', '1F9CD-1F3FF-200D-2640-FE0F', '1F9CD-1F3FF-200D-2642-FE0F', '1F9CD-1F3FF', '1F9CD-200D-2640-FE0F', '1F9CD-200D-2642-FE0F', '1F9CD', '1F9CE-1F3FB-200D-2640-FE0F', '1F9CE-1F3FB-200D-2642-FE0F', '1F9CE-1F3FB', '1F9CE-1F3FC-200D-2640-FE0F', '1F9CE-1F3FC-200D-2642-FE0F', '1F9CE-1F3FC', '1F9CE-1F3FD-200D-2640-FE0F', '1F9CE-1F3FD-200D-2642-FE0F', '1F9CE-1F3FD', '1F9CE-1F3FE-200D-2640-FE0F', '1F9CE-1F3FE-200D-2642-FE0F', '1F9CE-1F3FE', '1F9CE-1F3FF-200D-2640-FE0F', '1F9CE-1F3FF-200D-2642-FE0F', '1F9CE-1F3FF', '1F9CE-200D-2640-FE0F', '1F9CE-200D-2642-FE0F', '1F9CE', '1F9CF-1F3FB-200D-2640-FE0F', '1F9CF-1F3FB-200D-2642-FE0F', '1F9CF-1F3FB', '1F9CF-1F3FC-200D-2640-FE0F', '1F9CF-1F3FC-200D-2642-FE0F', '1F9CF-1F3FC', '1F9CF-1F3FD-200D-2640-FE0F', '1F9CF-1F3FD-200D-2642-FE0F', '1F9CF-1F3FD', '1F9CF-1F3FE-200D-2640-FE0F', '1F9CF-1F3FE-200D-2642-FE0F', '1F9CF-1F3FE', '1F9CF-1F3FF-200D-2640-FE0F', '1F9CF-1F3FF-200D-2642-FE0F', '1F9CF-1F3FF', '1F9CF-200D-2640-FE0F', '1F9CF-200D-2642-FE0F', '1F9CF', '1F9D0', '1F9D1-1F3FB-200D-1F33E', '1F9D1-1F3FB-200D-1F373', '1F9D1-1F3FB-200D-1F37C', '1F9D1-1F3FB-200D-1F384', '1F9D1-1F3FB-200D-1F393', '1F9D1-1F3FB-200D-1F3A4', '1F9D1-1F3FB-200D-1F3A8', '1F9D1-1F3FB-200D-1F3EB', '1F9D1-1F3FB-200D-1F3ED', '1F9D1-1F3FB-200D-1F4BB', '1F9D1-1F3FB-200D-1F4BC', '1F9D1-1F3FB-200D-1F527', '1F9D1-1F3FB-200D-1F52C', '1F9D1-1F3FB-200D-1F680', '1F9D1-1F3FB-200D-1F692', '1F9D1-1F3FB-200D-1F91D-200D-1F9D1-1F3FB', '1F9D1-1F3FB-200D-1F91D-200D-1F9D1-1F3FC', '1F9D1-1F3FB-200D-1F91D-200D-1F9D1-1F3FD', '1F9D1-1F3FB-200D-1F91D-200D-1F9D1-1F3FE', '1F9D1-1F3FB-200D-1F91D-200D-1F9D1-1F3FF', '1F9D1-1F3FB-200D-1F9AF', '1F9D1-1F3FB-200D-1F9B0', '1F9D1-1F3FB-200D-1F9B1', '1F9D1-1F3FB-200D-1F9B2', '1F9D1-1F3FB-200D-1F9B3', '1F9D1-1F3FB-200D-1F9BC', '1F9D1-1F3FB-200D-1F9BD', '1F9D1-1F3FB-200D-2695-FE0F', '1F9D1-1F3FB-200D-2696-FE0F', '1F9D1-1F3FB-200D-2708-FE0F', '1F9D1-1F3FB', '1F9D1-1F3FC-200D-1F33E', '1F9D1-1F3FC-200D-1F373', '1F9D1-1F3FC-200D-1F37C', '1F9D1-1F3FC-200D-1F384', '1F9D1-1F3FC-200D-1F393', '1F9D1-1F3FC-200D-1F3A4', '1F9D1-1F3FC-200D-1F3A8', '1F9D1-1F3FC-200D-1F3EB', '1F9D1-1F3FC-200D-1F3ED', '1F9D1-1F3FC-200D-1F4BB', '1F9D1-1F3FC-200D-1F4BC', '1F9D1-1F3FC-200D-1F527', '1F9D1-1F3FC-200D-1F52C', '1F9D1-1F3FC-200D-1F680', '1F9D1-1F3FC-200D-1F692', '1F9D1-1F3FC-200D-1F91D-200D-1F9D1-1F3FB', '1F9D1-1F3FC-200D-1F91D-200D-1F9D1-1F3FC', '1F9D1-1F3FC-200D-1F91D-200D-1F9D1-1F3FD', '1F9D1-1F3FC-200D-1F91D-200D-1F9D1-1F3FE', '1F9D1-1F3FC-200D-1F91D-200D-1F9D1-1F3FF', '1F9D1-1F3FC-200D-1F9AF', '1F9D1-1F3FC-200D-1F9B0', '1F9D1-1F3FC-200D-1F9B1', '1F9D1-1F3FC-200D-1F9B2', '1F9D1-1F3FC-200D-1F9B3', '1F9D1-1F3FC-200D-1F9BC', '1F9D1-1F3FC-200D-1F9BD', '1F9D1-1F3FC-200D-2695-FE0F', '1F9D1-1F3FC-200D-2696-FE0F', '1F9D1-1F3FC-200D-2708-FE0F', '1F9D1-1F3FC', '1F9D1-1F3FD-200D-1F33E', '1F9D1-1F3FD-200D-1F373', '1F9D1-1F3FD-200D-1F37C', '1F9D1-1F3FD-200D-1F384', '1F9D1-1F3FD-200D-1F393', '1F9D1-1F3FD-200D-1F3A4', '1F9D1-1F3FD-200D-1F3A8', '1F9D1-1F3FD-200D-1F3EB', '1F9D1-1F3FD-200D-1F3ED', '1F9D1-1F3FD-200D-1F4BB', '1F9D1-1F3FD-200D-1F4BC', '1F9D1-1F3FD-200D-1F527', '1F9D1-1F3FD-200D-1F52C', '1F9D1-1F3FD-200D-1F680', '1F9D1-1F3FD-200D-1F692', '1F9D1-1F3FD-200D-1F91D-200D-1F9D1-1F3FB', '1F9D1-1F3FD-200D-1F91D-200D-1F9D1-1F3FC', '1F9D1-1F3FD-200D-1F91D-200D-1F9D1-1F3FD', '1F9D1-1F3FD-200D-1F91D-200D-1F9D1-1F3FE', '1F9D1-1F3FD-200D-1F91D-200D-1F9D1-1F3FF', '1F9D1-1F3FD-200D-1F9AF', '1F9D1-1F3FD-200D-1F9B0', '1F9D1-1F3FD-200D-1F9B1', '1F9D1-1F3FD-200D-1F9B2', '1F9D1-1F3FD-200D-1F9B3', '1F9D1-1F3FD-200D-1F9BC', '1F9D1-1F3FD-200D-1F9BD', '1F9D1-1F3FD-200D-2695-FE0F', '1F9D1-1F3FD-200D-2696-FE0F', '1F9D1-1F3FD-200D-2708-FE0F', '1F9D1-1F3FD', '1F9D1-1F3FE-200D-1F33E', '1F9D1-1F3FE-200D-1F373', '1F9D1-1F3FE-200D-1F37C', '1F9D1-1F3FE-200D-1F384', '1F9D1-1F3FE-200D-1F393', '1F9D1-1F3FE-200D-1F3A4', '1F9D1-1F3FE-200D-1F3A8', '1F9D1-1F3FE-200D-1F3EB', '1F9D1-1F3FE-200D-1F3ED', '1F9D1-1F3FE-200D-1F4BB', '1F9D1-1F3FE-200D-1F4BC', '1F9D1-1F3FE-200D-1F527', '1F9D1-1F3FE-200D-1F52C', '1F9D1-1F3FE-200D-1F680', '1F9D1-1F3FE-200D-1F692', '1F9D1-1F3FE-200D-1F91D-200D-1F9D1-1F3FB', '1F9D1-1F3FE-200D-1F91D-200D-1F9D1-1F3FC', '1F9D1-1F3FE-200D-1F91D-200D-1F9D1-1F3FD', '1F9D1-1F3FE-200D-1F91D-200D-1F9D1-1F3FE', '1F9D1-1F3FE-200D-1F91D-200D-1F9D1-1F3FF', '1F9D1-1F3FE-200D-1F9AF', '1F9D1-1F3FE-200D-1F9B0', '1F9D1-1F3FE-200D-1F9B1', '1F9D1-1F3FE-200D-1F9B2', '1F9D1-1F3FE-200D-1F9B3', '1F9D1-1F3FE-200D-1F9BC', '1F9D1-1F3FE-200D-1F9BD', '1F9D1-1F3FE-200D-2695-FE0F', '1F9D1-1F3FE-200D-2696-FE0F', '1F9D1-1F3FE-200D-2708-FE0F', '1F9D1-1F3FE', '1F9D1-1F3FF-200D-1F33E', '1F9D1-1F3FF-200D-1F373', '1F9D1-1F3FF-200D-1F37C', '1F9D1-1F3FF-200D-1F384', '1F9D1-1F3FF-200D-1F393', '1F9D1-1F3FF-200D-1F3A4', '1F9D1-1F3FF-200D-1F3A8', '1F9D1-1F3FF-200D-1F3EB', '1F9D1-1F3FF-200D-1F3ED', '1F9D1-1F3FF-200D-1F4BB', '1F9D1-1F3FF-200D-1F4BC', '1F9D1-1F3FF-200D-1F527', '1F9D1-1F3FF-200D-1F52C', '1F9D1-1F3FF-200D-1F680', '1F9D1-1F3FF-200D-1F692', '1F9D1-1F3FF-200D-1F91D-200D-1F9D1-1F3FB', '1F9D1-1F3FF-200D-1F91D-200D-1F9D1-1F3FC', '1F9D1-1F3FF-200D-1F91D-200D-1F9D1-1F3FD', '1F9D1-1F3FF-200D-1F91D-200D-1F9D1-1F3FE', '1F9D1-1F3FF-200D-1F91D-200D-1F9D1-1F3FF', '1F9D1-1F3FF-200D-1F9AF', '1F9D1-1F3FF-200D-1F9B0', '1F9D1-1F3FF-200D-1F9B1', '1F9D1-1F3FF-200D-1F9B2', '1F9D1-1F3FF-200D-1F9B3', '1F9D1-1F3FF-200D-1F9BC', '1F9D1-1F3FF-200D-1F9BD', '1F9D1-1F3FF-200D-2695-FE0F', '1F9D1-1F3FF-200D-2696-FE0F', '1F9D1-1F3FF-200D-2708-FE0F', '1F9D1-1F3FF', '1F9D1-200D-1F33E', '1F9D1-200D-1F373', '1F9D1-200D-1F37C', '1F9D1-200D-1F384', '1F9D1-200D-1F393', '1F9D1-200D-1F3A4', '1F9D1-200D-1F3A8', '1F9D1-200D-1F3EB', '1F9D1-200D-1F3ED', '1F9D1-200D-1F4BB', '1F9D1-200D-1F4BC', '1F9D1-200D-1F527', '1F9D1-200D-1F52C', '1F9D1-200D-1F680', '1F9D1-200D-1F692', '1F9D1-200D-1F91D-200D-1F9D1', '1F9D1-200D-1F9AF', '1F9D1-200D-1F9B0', '1F9D1-200D-1F9B1', '1F9D1-200D-1F9B2', '1F9D1-200D-1F9B3', '1F9D1-200D-1F9BC', '1F9D1-200D-1F9BD', '1F9D1-200D-2695-FE0F', '1F9D1-200D-2696-FE0F', '1F9D1-200D-2708-FE0F', '1F9D1', '1F9D2-1F3FB', '1F9D2-1F3FC', '1F9D2-1F3FD', '1F9D2-1F3FE', '1F9D2-1F3FF', '1F9D2', '1F9D3-1F3FB', '1F9D3-1F3FC', '1F9D3-1F3FD', '1F9D3-1F3FE', '1F9D3-1F3FF', '1F9D3', '1F9D4-1F3FB', '1F9D4-1F3FC', '1F9D4-1F3FD', '1F9D4-1F3FE', '1F9D4-1F3FF', '1F9D4', '1F9D5-1F3FB', '1F9D5-1F3FC', '1F9D5-1F3FD', '1F9D5-1F3FE', '1F9D5-1F3FF', '1F9D5', '1F9D6-1F3FB-200D-2640-FE0F', '1F9D6-1F3FB-200D-2642-FE0F', '1F9D6-1F3FB', '1F9D6-1F3FC-200D-2640-FE0F', '1F9D6-1F3FC-200D-2642-FE0F', '1F9D6-1F3FC', '1F9D6-1F3FD-200D-2640-FE0F', '1F9D6-1F3FD-200D-2642-FE0F', '1F9D6-1F3FD', '1F9D6-1F3FE-200D-2640-FE0F', '1F9D6-1F3FE-200D-2642-FE0F', '1F9D6-1F3FE', '1F9D6-1F3FF-200D-2640-FE0F', '1F9D6-1F3FF-200D-2642-FE0F', '1F9D6-1F3FF', '1F9D6-200D-2640-FE0F', '1F9D6-200D-2642-FE0F', '1F9D6', '1F9D7-1F3FB-200D-2640-FE0F', '1F9D7-1F3FB-200D-2642-FE0F', '1F9D7-1F3FB', '1F9D7-1F3FC-200D-2640-FE0F', '1F9D7-1F3FC-200D-2642-FE0F', '1F9D7-1F3FC', '1F9D7-1F3FD-200D-2640-FE0F', '1F9D7-1F3FD-200D-2642-FE0F', '1F9D7-1F3FD', '1F9D7-1F3FE-200D-2640-FE0F', '1F9D7-1F3FE-200D-2642-FE0F', '1F9D7-1F3FE', '1F9D7-1F3FF-200D-2640-FE0F', '1F9D7-1F3FF-200D-2642-FE0F', '1F9D7-1F3FF', '1F9D7-200D-2640-FE0F', '1F9D7-200D-2642-FE0F', '1F9D7', '1F9D8-1F3FB-200D-2640-FE0F', '1F9D8-1F3FB-200D-2642-FE0F', '1F9D8-1F3FB', '1F9D8-1F3FC-200D-2640-FE0F', '1F9D8-1F3FC-200D-2642-FE0F', '1F9D8-1F3FC', '1F9D8-1F3FD-200D-2640-FE0F', '1F9D8-1F3FD-200D-2642-FE0F', '1F9D8-1F3FD', '1F9D8-1F3FE-200D-2640-FE0F', '1F9D8-1F3FE-200D-2642-FE0F', '1F9D8-1F3FE', '1F9D8-1F3FF-200D-2640-FE0F', '1F9D8-1F3FF-200D-2642-FE0F', '1F9D8-1F3FF', '1F9D8-200D-2640-FE0F', '1F9D8-200D-2642-FE0F', '1F9D8', '1F9D9-1F3FB-200D-2640-FE0F', '1F9D9-1F3FB-200D-2642-FE0F', '1F9D9-1F3FB', '1F9D9-1F3FC-200D-2640-FE0F', '1F9D9-1F3FC-200D-2642-FE0F', '1F9D9-1F3FC', '1F9D9-1F3FD-200D-2640-FE0F', '1F9D9-1F3FD-200D-2642-FE0F', '1F9D9-1F3FD', '1F9D9-1F3FE-200D-2640-FE0F', '1F9D9-1F3FE-200D-2642-FE0F', '1F9D9-1F3FE', '1F9D9-1F3FF-200D-2640-FE0F', '1F9D9-1F3FF-200D-2642-FE0F', '1F9D9-1F3FF', '1F9D9-200D-2640-FE0F', '1F9D9-200D-2642-FE0F', '1F9D9', '1F9DA-1F3FB-200D-2640-FE0F', '1F9DA-1F3FB-200D-2642-FE0F', '1F9DA-1F3FB', '1F9DA-1F3FC-200D-2640-FE0F', '1F9DA-1F3FC-200D-2642-FE0F', '1F9DA-1F3FC', '1F9DA-1F3FD-200D-2640-FE0F', '1F9DA-1F3FD-200D-2642-FE0F', '1F9DA-1F3FD', '1F9DA-1F3FE-200D-2640-FE0F', '1F9DA-1F3FE-200D-2642-FE0F', '1F9DA-1F3FE', '1F9DA-1F3FF-200D-2640-FE0F', '1F9DA-1F3FF-200D-2642-FE0F', '1F9DA-1F3FF', '1F9DA-200D-2640-FE0F', '1F9DA-200D-2642-FE0F', '1F9DA', '1F9DB-1F3FB-200D-2640-FE0F', '1F9DB-1F3FB-200D-2642-FE0F', '1F9DB-1F3FB', '1F9DB-1F3FC-200D-2640-FE0F', '1F9DB-1F3FC-200D-2642-FE0F', '1F9DB-1F3FC', '1F9DB-1F3FD-200D-2640-FE0F', '1F9DB-1F3FD-200D-2642-FE0F', '1F9DB-1F3FD', '1F9DB-1F3FE-200D-2640-FE0F', '1F9DB-1F3FE-200D-2642-FE0F', '1F9DB-1F3FE', '1F9DB-1F3FF-200D-2640-FE0F', '1F9DB-1F3FF-200D-2642-FE0F', '1F9DB-1F3FF', '1F9DB-200D-2640-FE0F', '1F9DB-200D-2642-FE0F', '1F9DB', '1F9DC-1F3FB-200D-2640-FE0F', '1F9DC-1F3FB-200D-2642-FE0F', '1F9DC-1F3FB', '1F9DC-1F3FC-200D-2640-FE0F', '1F9DC-1F3FC-200D-2642-FE0F', '1F9DC-1F3FC', '1F9DC-1F3FD-200D-2640-FE0F', '1F9DC-1F3FD-200D-2642-FE0F', '1F9DC-1F3FD', '1F9DC-1F3FE-200D-2640-FE0F', '1F9DC-1F3FE-200D-2642-FE0F', '1F9DC-1F3FE', '1F9DC-1F3FF-200D-2640-FE0F', '1F9DC-1F3FF-200D-2642-FE0F', '1F9DC-1F3FF', '1F9DC-200D-2640-FE0F', '1F9DC-200D-2642-FE0F', '1F9DC', '1F9DD-1F3FB-200D-2640-FE0F', '1F9DD-1F3FB-200D-2642-FE0F', '1F9DD-1F3FB', '1F9DD-1F3FC-200D-2640-FE0F', '1F9DD-1F3FC-200D-2642-FE0F', '1F9DD-1F3FC', '1F9DD-1F3FD-200D-2640-FE0F', '1F9DD-1F3FD-200D-2642-FE0F', '1F9DD-1F3FD', '1F9DD-1F3FE-200D-2640-FE0F', '1F9DD-1F3FE-200D-2642-FE0F', '1F9DD-1F3FE', '1F9DD-1F3FF-200D-2640-FE0F', '1F9DD-1F3FF-200D-2642-FE0F', '1F9DD-1F3FF', '1F9DD-200D-2640-FE0F', '1F9DD-200D-2642-FE0F', '1F9DD', '1F9DE-200D-2640-FE0F', '1F9DE-200D-2642-FE0F', '1F9DE', '1F9DF-200D-2640-FE0F', '1F9DF-200D-2642-FE0F', '1F9DF', '1F9E0', '1F9E1', '1F9E2', '1F9E3', '1F9E4', '1F9E5', '1F9E6', '1F9E7', '1F9E8', '1F9E9', '1F9EA', '1F9EB', '1F9EC', '1F9ED', '1F9EE', '1F9EF', '1F9F0', '1F9F1', '1F9F2', '1F9F3', '1F9F4', '1F9F5', '1F9F6', '1F9F7', '1F9F8', '1F9F9', '1F9FA', '1F9FB', '1F9FC', '1F9FD', '1F9FE', '1F9FF', '1FA70', '1FA71', '1FA72', '1FA73', '1FA74', '1FA78', '1FA79', '1FA7A', '1FA80', '1FA81', '1FA82', '1FA83', '1FA84', '1FA85', '1FA86', '1FA90', '1FA91', '1FA92', '1FA93', '1FA94', '1FA95', '1FA96', '1FA97', '1FA98', '1FA99', '1FA9A', '1FA9B', '1FA9C', '1FA9D', '1FA9E', '1FA9F', '1FAA0', '1FAA1', '1FAA2', '1FAA3', '1FAA4', '1FAA5', '1FAA6', '1FAA7', '1FAA8', '1FAB0', '1FAB1', '1FAB2', '1FAB3', '1FAB4', '1FAB5', '1FAB6', '1FAC0', '1FAC1', '1FAC2', '1FAD0', '1FAD1', '1FAD2', '1FAD3', '1FAD4', '1FAD5', '1FAD6', '1FBC5', '1FBC6-200D-1F457', '1FBC6', '1FBC7-200D-1F457', '1FBC7', '1FBC8-200D-1F457', '1FBC8', '1FBC9', '1FEF4-E0065-E0073-E0063-E0074-E007F', '203C', '2049', '2117', '2120', '2122', '2139', '2194', '2195', '2196', '2197', '2198', '2199', '21A9', '21AA', '229C', '231A', '231B', '2328', '23CF', '23E9', '23EA', '23EB', '23EC', '23ED', '23EE', '23EF', '23F0', '23F1', '23F2', '23F3', '23F8', '23F9', '23FA', '23FB', '23FC', '23FD', '23FE', '24C2', '25A1', '25AA', '25AB', '25AC', '25AD', '25B6', '25C0', '25D0', '25D1', '25E7', '25E8', '25E9', '25EA', '25ED', '25EE', '25FB', '25FC', '25FD', '25FE', '2600', '2601', '2602', '2603', '2604', '2605', '260E', '2611', '2614', '2615', '2618', '261D-1F3FB', '261D-1F3FC', '261D-1F3FD', '261D-1F3FE', '261D-1F3FF', '261D', '2620', '2622', '2623', '2626', '262A', '262E', '262F', '2638', '2639', '263A', '2640', '2642', '2648', '2649', '264A', '264B', '264C', '264D', '264E', '264F', '2650', '2651', '2652', '2653', '265F', '2660', '2663', '2665', '2666', '2668', '267B', '267E', '267F', '2691-FE0F-200D-1F7E5', '2691-FE0F-200D-1F7E6', '2691-FE0F-200D-1F7E7', '2691-FE0F-200D-1F7E8', '2691-FE0F-200D-1F7E9-200D-2605-FE0F', '2691-FE0F-200D-1F7E9', '2691-FE0F-200D-1F7EA', '2691-FE0F-200D-1F7EB', '2692', '2693', '2694', '2695', '2696', '2697', '2699', '269B', '269C', '26A0', '26A1', '26A7', '26AA', '26AB', '26B0', '26B1', '26BD', '26BE', '26C4', '26C5', '26C8', '26CE', '26CF', '26D1', '26D3', '26D4', '26E9', '26EA', '26F0', '26F1', '26F2', '26F3', '26F4', '26F5', '26F7', '26F8', '26F9-1F3FB-200D-2640-FE0F', '26F9-1F3FB-200D-2642-FE0F', '26F9-1F3FB', '26F9-1F3FC-200D-2640-FE0F', '26F9-1F3FC-200D-2642-FE0F', '26F9-1F3FC', '26F9-1F3FD-200D-2640-FE0F', '26F9-1F3FD-200D-2642-FE0F', '26F9-1F3FD', '26F9-1F3FE-200D-2640-FE0F', '26F9-1F3FE-200D-2642-FE0F', '26F9-1F3FE', '26F9-1F3FF-200D-2640-FE0F', '26F9-1F3FF-200D-2642-FE0F', '26F9-1F3FF', '26F9-FE0F-200D-2640-FE0F', '26F9-FE0F-200D-2642-FE0F', '26F9', '26FA', '26FD', '2702', '2705', '2708', '2709', '270A-1F3FB', '270A-1F3FC', '270A-1F3FD', '270A-1F3FE', '270A-1F3FF', '270A', '270B-1F3FB', '270B-1F3FC', '270B-1F3FD', '270B-1F3FE', '270B-1F3FF', '270B', '270C-1F3FB', '270C-1F3FC', '270C-1F3FD', '270C-1F3FE', '270C-1F3FF', '270C', '270D-1F3FB', '270D-1F3FC', '270D-1F3FD', '270D-1F3FE', '270D-1F3FF', '270D', '270F', '2712', '2714', '2716', '271D', '2721', '2728', '2733', '2734', '2744', '2747', '274C', '274E', '2753', '2754', '2755', '2757', '2763', '2764', '2795', '2796', '2797', '27A1', '27B0', '27BF', '2934', '2935', '2B05', '2B06', '2B07', '2B0C', '2B0D', '2B1B', '2B1C', '2B1F', '2B20', '2B21', '2B22', '2B23', '2B24', '2B2E', '2B2F', '2B50', '2B55', '2B58', '2B8F', '2BBA', '2BBB', '2BBC', '2BC3', '2BC4', '2BEA', '2BEB', '3030', '303D', '3297', '3299', 'E000', 'E001', 'E002', 'E003', 'E004', 'E005', 'E006', 'E007', 'E008', 'E009', 'E010', 'E011', 'E040', 'E041', 'E042', 'E043', 'E044', 'E045', 'E046', 'E047', 'E048', 'E049', 'E04A', 'E04B', 'E050', 'E051', 'E052', 'E053', 'E054', 'E055', 'E056', 'E057', 'E080', 'E081', 'E082', 'E083', 'E084', 'E085', 'E086', 'E087', 'E088', 'E089', 'E08A', 'E08B', 'E08C', 'E08D', 'E08E', 'E08F', 'E090', 'E091', 'E092', 'E093', 'E094', 'E095', 'E096', 'E097', 'E098', 'E099', 'E09A', 'E09B', 'E09C', 'E09D', 'E09E', 'E09F', 'E0A0', 'E0A1', 'E0A2', 'E0A3', 'E0A4', 'E0A5', 'E0A6', 'E0A7', 'E0A8', 'E0A9', 'E0AA', 'E0AB', 'E0AC-200D-2640-FE0F', 'E0AC-200D-2642-FE0F', 'E0AC', 'E0AD-200D-2640-FE0F', 'E0AD-200D-2642-FE0F', 'E0AD', 'E0AE', 'E0AF', 'E0B0', 'E0B1', 'E0B2', 'E0B3', 'E0B4', 'E0C0', 'E0C1', 'E0C2', 'E0C3', 'E0C4', 'E0C5', 'E0C6', 'E0FF', 'E100', 'E101', 'E102', 'E103', 'E104', 'E105', 'E106', 'E107', 'E108', 'E109', 'E10A', 'E10B', 'E10C', 'E10D', 'E140', 'E141', 'E142', 'E143', 'E144', 'E146', 'E147', 'E148', 'E149', 'E150', 'E151', 'E152', 'E153', 'E154', 'E155', 'E156', 'E157', 'E181', 'E182', 'E183', 'E184', 'E185', 'E186', 'E187', 'E188', 'E1C0', 'E1C1', 'E1C2', 'E1C3', 'E1C4', 'E1C6', 'E1C7', 'E1C8', 'E1C9', 'E1CA', 'E1CB', 'E1CC', 'E1CD', 'E1CE', 'E1CF', 'E1D0', 'E1D1', 'E1D2', 'E1D3', 'E1D4', 'E1D5', 'E1D6', 'E1D7', 'E200', 'E201', 'E202', 'E203', 'E204', 'E205', 'E206', 'E207', 'E208', 'E209', 'E20A', 'E20B', 'E20C', 'E20D', 'E240', 'E241', 'E242', 'E243', 'E244', 'E245', 'E246', 'E247', 'E248', 'E249', 'E24A', 'E24B', 'E24C', 'E24D', 'E24E', 'E24F', 'E250', 'E251', 'E252', 'E253', 'E254', 'E255', 'E256', 'E257', 'E258', 'E259', 'E25A', 'E25B', 'E25C', 'E25D', 'E25E', 'E25F', 'E260', 'E261', 'E262', 'E263', 'E264', 'E265', 'E266', 'E267', 'E268', 'E269', 'E280', 'E281', 'E282', 'E2C0', 'E2C1', 'E2C2', 'E2C3', 'E2C4', 'E2C6', 'E2C7', 'E2C8', 'E2C9', 'E2CA', 'E2CB', 'E2CC', 'E2CD', 'E2CE', 'E2CF', 'E2D0', 'E2D1', 'E2D2', 'E2D3', 'E2D4', 'E2D5', 'E2D6', 'E2D7', 'E2D8', 'E2D9', 'E2DA', 'E300', 'E301', 'E302', 'E303', 'E305', 'E306', 'E307', 'E308', 'E309', 'E30A', 'E30B', 'E30C', 'E30D', 'E30E', 'E30F', 'E312', 'E313', 'E314', 'E315', 'E316', 'E318', 'E319', 'E31A', 'E31B', 'E31C', 'E31D', 'E31E', 'E31F', 'E320', 'E321', 'E322', 'E324', 'E325', 'E326', 'E327', 'E328', 'E329', 'E32B', 'E340', 'E341', 'E342', 'E343', 'E344', 'E345', 'E346', 'E347', 'E348', 'E380', 'E381', 'F000', 'F8FF']);
var $author$project$Chat$Join = 1;
var $author$project$Chat$mkJoinMsg = A2(
	$elm$core$Basics$composeL,
	A2(
		$elm$core$Basics$composeL,
		$author$project$Chat$mkWsMsg(1),
		$joneshf$elm_tagged$Tagged$tag),
	$joneshf$elm_tagged$Tagged$untag);
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
var $author$project$Chat$ChangingTo = function (a) {
	return {$: 1, a: a};
};
var $author$project$Chat$ChatError = function (a) {
	return {$: 3, a: a};
};
var $author$project$Chat$DomError = function (a) {
	return {$: 4, a: a};
};
var $author$project$Chat$GotEmojiViewport = function (a) {
	return {$: 11, a: a};
};
var $author$project$Chat$GotInputTime = function (a) {
	return {$: 5, a: a};
};
var $author$project$Chat$GotViewport = function (a) {
	return {$: 2, a: a};
};
var $author$project$Chat$OnMsgsViewEvent = function (a) {
	return {$: 6, a: a};
};
var $author$project$Chat$TypeHint = 3;
var $author$project$Chat$Typing = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
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
					task)));
	});
var $author$project$Chat$autoScrollMargin = 30;
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Utils$Utils$durationSec = F2(
	function (startTime, endTime) {
		return $elm$core$Basics$round(
			($elm$time$Time$posixToMillis(endTime) - $elm$time$Time$posixToMillis(startTime)) / 1000);
	});
var $author$project$Views$Chat$emojiPickerHtmlId = 'emoji-picker';
var $elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var $author$project$Utils$Utils$hasManualScrolledUp = F2(
	function (viewport, margin) {
		return (_Utils_cmp((viewport.fe.fj + viewport.fe.bz) + margin, viewport.em.bz) < 0) ? true : false;
	});
var $author$project$Chat$NameChange = 2;
var $author$project$Chat$mkNameChangeMsg = $author$project$Chat$mkWsMsg(2);
var $author$project$Chat$mkTypeHintMsg = function (isTyping) {
	var bodyStr = function () {
		if (isTyping) {
			return 'start';
		} else {
			return 'stop';
		}
	}();
	return A2(
		$author$project$Chat$mkWsMsg,
		3,
		$joneshf$elm_tagged$Tagged$tag(bodyStr));
};
var $author$project$Views$Chat$msgsViewHtmlId = 'chat-msgs-view';
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Views$Chat$port_NotifyChat = _Platform_outgoingPort(
	'port_NotifyChat',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $elm_community$list_extra$List$Extra$remove = F2(
	function (x, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var y = xs.a;
			var ys = xs.b;
			return _Utils_eq(x, y) ? ys : A2(
				$elm$core$List$cons,
				y,
				A2($elm_community$list_extra$List$Extra$remove, x, ys));
		}
	});
var $author$project$Chat$TriedSnapScroll = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$Dom$setViewportOf = _Browser_setViewportOf;
var $author$project$Views$Chat$snapScrollChatMsgsView = A3(
	$elm$core$Basics$apL,
	$elm$core$Task$attempt,
	A2($elm$core$Basics$composeL, $author$project$Chat$OnMsgsViewEvent, $author$project$Chat$TriedSnapScroll),
	A2(
		$elm$core$Task$andThen,
		function (viewport) {
			return A3($elm$browser$Browser$Dom$setViewportOf, $author$project$Views$Chat$msgsViewHtmlId, 0, viewport.em.bz);
		},
		$elm$browser$Browser$Dom$getViewportOf($author$project$Views$Chat$msgsViewHtmlId)));
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_community$list_extra$List$Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$Set$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$Set$insert, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$unique = function (list) {
	return A4($elm_community$list_extra$List$Extra$uniqueHelp, $elm$core$Basics$identity, $elm$core$Set$empty, list, _List_Nil);
};
var $author$project$Chat$ChatMsgMeta_ = function (a) {
	return {$: 0, a: a};
};
var $author$project$Chat$CtrlMsg_ = function (a) {
	return {$: 1, a: a};
};
var $author$project$Chat$MsgHistory_ = function (a) {
	return {$: 2, a: a};
};
var $author$project$Chat$UserIdMsg_ = function (a) {
	return {$: 3, a: a};
};
var $author$project$Chat$ChatMsgMeta = F4(
	function (msgFromClient, userId, username, posixTimeSec) {
		return {bN: msgFromClient, bY: posixTimeSec, af: userId, aQ: username};
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Chat$Leave = 4;
var $author$project$Chat$MsgFromClient = F2(
	function (msgType, msgBody) {
		return {dT: msgBody, bO: msgType};
	});
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Chat$msgFromClientDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Chat$MsgFromClient,
	A2(
		$elm$json$Json$Decode$andThen,
		function (str) {
			return (str === 'content') ? $elm$json$Json$Decode$succeed(0) : ((str === 'join') ? $elm$json$Json$Decode$succeed(1) : ((str === 'nameChange') ? $elm$json$Json$Decode$succeed(2) : ((str === 'typeHint') ? $elm$json$Json$Decode$succeed(3) : ((str === 'leave') ? $elm$json$Json$Decode$succeed(4) : $elm$json$Json$Decode$fail('Invalid MsgType')))));
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
var $author$project$Chat$Err_ = $elm$core$Basics$identity;
var $author$project$Chat$MaxJoined = 0;
var $author$project$Chat$NotFound = 1;
var $author$project$Chat$errCtrlMsgDecoder = A2(
	$elm$json$Json$Decode$map,
	$elm$core$Basics$identity,
	A2(
		$elm$json$Json$Decode$andThen,
		function (str) {
			return (str === 'maxJoined') ? $elm$json$Json$Decode$succeed(0) : ((str === 'notFound') ? $elm$json$Json$Decode$succeed(1) : $elm$json$Json$Decode$fail('Invalid error ctrl msg type'));
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
		return {bL: maxJoinCount, L: msgs, ck: users};
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
			if (!_v2.$) {
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
	return {fk: yourUserId};
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
var $author$project$Views$Chat$updateModel = F3(
	function (elmMsg, model, windowVisibility) {
		switch (elmMsg.$) {
			case 0:
				var str = elmMsg.a;
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{
								dw: $joneshf$elm_tagged$Tagged$tag(str)
							})),
					A2($elm$core$String$endsWith, '\n', str) ? $elm$core$Platform$Cmd$none : A2($elm$core$Task$perform, $author$project$Chat$GotInputTime, $elm$time$Time$now));
			case 1:
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{e8: $author$project$Chat$NotTyping})),
					$author$project$Views$Chat$sendChatMsg(model.dw));
			case 3:
				var str = elmMsg.a;
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{
								dW: $author$project$Chat$ChangingTo(str)
							})),
					$elm$core$Platform$Cmd$none);
			case 2:
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{
								dW: $author$project$Chat$ChangingTo('')
							})),
					$elm$core$Platform$Cmd$none);
			case 4:
				var isConfirmed = elmMsg.a;
				var _v1 = model.dW;
				if (_v1.$ === 1) {
					var newNameInput = _v1.a;
					return (!isConfirmed) ? _Utils_Tuple2(
						$author$project$Chat$Normal(
							_Utils_update(
								model,
								{dW: $author$project$Chat$NotChanging})),
						$elm$core$Platform$Cmd$none) : ($elm$core$String$isEmpty(newNameInput) ? _Utils_Tuple2(
						$author$project$Chat$Normal(model),
						$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
						$author$project$Chat$Normal(
							_Utils_update(
								model,
								{dW: $author$project$Chat$NotChanging})),
						$author$project$Views$Chat$port_SendWsMsg(
							$author$project$Chat$mkNameChangeMsg(
								$joneshf$elm_tagged$Tagged$tag(newNameInput)))));
				} else {
					return _Utils_Tuple2(
						$author$project$Chat$Normal(model),
						$elm$core$Platform$Cmd$none);
				}
			case 5:
				var inputTime = elmMsg.a;
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{
								e8: $author$project$Chat$Typing(inputTime)
							})),
					_Utils_eq(model.e8, $author$project$Chat$NotTyping) ? $author$project$Views$Chat$port_SendWsMsg(
						$author$project$Chat$mkTypeHintMsg(true)) : $elm$core$Platform$Cmd$none);
			case 6:
				var event = elmMsg.a;
				switch (event.$) {
					case 0:
						var result = event.a;
						return _Utils_Tuple2(
							$author$project$Chat$Normal(model),
							$elm$core$Platform$Cmd$none);
					case 1:
						return _Utils_Tuple2(
							$author$project$Chat$Normal(model),
							A2(
								$elm$core$Task$attempt,
								A2($elm$core$Basics$composeL, $author$project$Chat$OnMsgsViewEvent, $author$project$Chat$GotViewport),
								$elm$browser$Browser$Dom$getViewportOf($author$project$Views$Chat$msgsViewHtmlId)));
					case 2:
						var result = event.a;
						if (result.$ === 1) {
							var err = result.a;
							return _Utils_Tuple2(
								$author$project$Chat$Normal(model),
								$elm$core$Platform$Cmd$none);
						} else {
							var viewport = result.a;
							var hasManualScrolledUp = A2($author$project$Utils$Utils$hasManualScrolledUp, viewport, $author$project$Chat$autoScrollMargin);
							return _Utils_Tuple2(
								$author$project$Chat$Normal(
									_Utils_update(
										model,
										{dk: hasManualScrolledUp, et: model.et && hasManualScrolledUp})),
								$elm$core$Platform$Cmd$none);
						}
					default:
						return _Utils_Tuple2(
							$author$project$Chat$Normal(
								_Utils_update(
									model,
									{dk: false, et: false})),
							$author$project$Views$Chat$snapScrollChatMsgsView);
				}
			case 7:
				var isFocused = elmMsg.a;
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{dE: isFocused})),
					$elm$core$Platform$Cmd$none);
			case 8:
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{dD: !model.dD})),
					$elm$core$Platform$Cmd$none);
			case 9:
				var hex = elmMsg.a;
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{
								dw: $joneshf$elm_tagged$Tagged$tag(
									$joneshf$elm_tagged$Tagged$untag(model.dw) + (':' + (hex + ':')))
							})),
					$elm$core$Platform$Cmd$none);
			case 10:
				return _Utils_Tuple2(
					$author$project$Chat$Normal(model),
					A2(
						$elm$core$Task$attempt,
						$author$project$Chat$GotEmojiViewport,
						$elm$browser$Browser$Dom$getViewportOf($author$project$Views$Chat$emojiPickerHtmlId)));
			case 11:
				var result = elmMsg.a;
				if (result.$ === 1) {
					var err = result.a;
					return _Utils_Tuple2(
						$author$project$Chat$DomError($author$project$Views$Chat$emojiPickerHtmlId),
						$elm$core$Platform$Cmd$none);
				} else {
					var viewport = result.a;
					var shouldLoadMoreEmojis = _Utils_cmp(viewport.fe.fj, (viewport.em.bz - viewport.fe.bz) - 50) > 0;
					var newEmojisBuffer = (!shouldLoadMoreEmojis) ? model.dc : A2(
						$elm$core$List$take,
						$elm$core$List$length(model.dc) + 100,
						$author$project$Emoji$allHex);
					return _Utils_Tuple2(
						$author$project$Chat$Normal(
							_Utils_update(
								model,
								{dc: newEmojisBuffer})),
						$elm$core$Platform$Cmd$none);
				}
			case 12:
				return _Utils_Tuple2(
					$author$project$Chat$Normal(model),
					$elm$core$Platform$Cmd$none);
			case 13:
				return _Utils_Tuple2($author$project$Chat$WsError, $elm$core$Platform$Cmd$none);
			case 14:
				var str = elmMsg.a;
				var _v5 = A2($elm$json$Json$Decode$decodeString, $author$project$Chat$wsMsgDecoder, str);
				if (_v5.$ === 1) {
					return _Utils_Tuple2(
						$author$project$Chat$Normal(model),
						$elm$core$Platform$Cmd$none);
				} else {
					var wsMsg = _v5.a;
					switch (wsMsg.$) {
						case 0:
							var chatMsgMeta = wsMsg.a;
							var senderName = chatMsgMeta.aQ;
							var senderId = chatMsgMeta.af;
							var oldUsers = model.ck;
							var msgFromClient = chatMsgMeta.bN;
							var newMsgs = function () {
								var _v9 = msgFromClient.bO;
								if (_v9 === 3) {
									return model.L;
								} else {
									return _Utils_ap(
										model.L,
										_List_fromArray(
											[chatMsgMeta]));
								}
							}();
							var newTypingUsers = function () {
								var senderRemoved = A2($elm_community$list_extra$List$Extra$remove, senderId, model.e9);
								var _v8 = msgFromClient.bO;
								switch (_v8) {
									case 3:
										return ($joneshf$elm_tagged$Tagged$untag(msgFromClient.dT) === 'start') ? $elm_community$list_extra$List$Extra$unique(
											_Utils_ap(
												model.e9,
												_List_fromArray(
													[senderId]))) : senderRemoved;
									case 4:
										return senderRemoved;
									case 0:
										return senderRemoved;
									default:
										return model.e9;
								}
							}();
							var newUsers = function () {
								var _v7 = msgFromClient.bO;
								switch (_v7) {
									case 1:
										return A3($elm$core$Dict$insert, senderId, senderName, oldUsers);
									case 2:
										return A3(
											$elm$core$Dict$insert,
											senderId,
											$joneshf$elm_tagged$Tagged$untag(msgFromClient.dT),
											oldUsers);
									case 4:
										return A2($elm$core$Dict$remove, senderId, oldUsers);
									default:
										return model.ck;
								}
							}();
							var isMyMsg = _Utils_eq(chatMsgMeta.af, model.dV);
							var newTypingStatus = ((!msgFromClient.bO) && isMyMsg) ? $author$project$Chat$NotTyping : model.e8;
							return _Utils_Tuple2(
								$author$project$Chat$Normal(
									_Utils_update(
										model,
										{
											dw: (isMyMsg && (!msgFromClient.bO)) ? $joneshf$elm_tagged$Tagged$tag('') : model.dw,
											dI: (msgFromClient.bO === 1) ? (senderId + 1) : model.dI,
											L: newMsgs,
											et: model.et || (model.dk && ((!isMyMsg) && (msgFromClient.bO !== 3))),
											e8: newTypingStatus,
											e9: newTypingUsers,
											ck: newUsers
										})),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											(!model.dk) ? $author$project$Views$Chat$snapScrollChatMsgsView : $elm$core$Platform$Cmd$none,
											((windowVisibility === 1) && (msgFromClient.bO !== 3)) ? $author$project$Views$Chat$port_NotifyChat(0) : $elm$core$Platform$Cmd$none
										])));
						case 1:
							var ctrlMsg = wsMsg.a;
							var err = ctrlMsg;
							return _Utils_Tuple2(
								$author$project$Chat$ChatError(err),
								$elm$core$Platform$Cmd$none);
						case 2:
							var msgHistory = wsMsg.a;
							return _Utils_Tuple2(
								$author$project$Chat$Normal(
									_Utils_update(
										model,
										{bL: msgHistory.bL, L: msgHistory.L, ck: msgHistory.ck})),
								$elm$core$Platform$Cmd$none);
						default:
							var userIdMsg = wsMsg.a;
							return _Utils_Tuple2(
								$author$project$Chat$Normal(
									_Utils_update(
										model,
										{dV: userIdMsg.fk})),
								$elm$core$Platform$Cmd$none);
					}
				}
			default:
				var time = elmMsg.a;
				var newTypingStatus = function () {
					var _v11 = model.e8;
					if (_v11.$ === 1) {
						return $author$project$Chat$NotTyping;
					} else {
						var lastInputTime = _v11.a;
						return (A2($author$project$Utils$Utils$durationSec, lastInputTime, time) >= 5) ? $author$project$Chat$NotTyping : model.e8;
					}
				}();
				return _Utils_Tuple2(
					$author$project$Chat$Normal(
						_Utils_update(
							model,
							{e8: newTypingStatus})),
					((!_Utils_eq(model.e8, $author$project$Chat$NotTyping)) && _Utils_eq(newTypingStatus, $author$project$Chat$NotTyping)) ? $author$project$Views$Chat$port_SendWsMsg(
						$author$project$Chat$mkTypeHintMsg(false)) : $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Views$Chat$update = F3(
	function (elmMsg, status, windowVisibility) {
		switch (status.$) {
			case 0:
				var model = status.a;
				return A3($author$project$Views$Chat$updateModel, elmMsg, model, windowVisibility);
			case 1:
				var chatId = status.a;
				switch (elmMsg.$) {
					case 12:
						return _Utils_Tuple2(
							$author$project$Chat$Normal(
								{
									cW: chatId,
									dc: A2($elm$core$List$take, 100, $author$project$Emoji$allHex),
									dk: false,
									dw: $joneshf$elm_tagged$Tagged$tag(''),
									dD: false,
									dE: false,
									dF: false,
									dI: 0,
									bL: $elm$core$Maybe$Nothing,
									L: _List_Nil,
									dV: -1,
									dW: $author$project$Chat$NotChanging,
									et: false,
									e8: $author$project$Chat$NotTyping,
									e9: _List_Nil,
									ck: $elm$core$Dict$empty
								}),
							$author$project$Views$Chat$port_SendWsMsg(
								$author$project$Chat$mkJoinMsg(chatId)));
					case 13:
						return _Utils_Tuple2($author$project$Chat$WsError, $elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(status, $elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(status, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Letter$BadMaxReadCount = 1;
var $author$project$Letter$EmptyBody = 0;
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
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
			var _v0 = $author$project$Utils$Types$strToPosIntInput(input_.am);
			if (_v0.$ === 1) {
				return $elm$core$Result$Err(1);
			} else {
				var posInt = _v0.a;
				return $elm$core$Result$Ok(
					{aa: input_.aa, am: posInt});
			}
		},
		function (input_) {
			return $elm$core$String$isEmpty(input_.aa) ? $elm$core$Result$Err(0) : $elm$core$Result$Ok(input_);
		}(input));
};
var $author$project$Main$updateModel = F2(
	function (msg, model) {
		var letterRawInput = model.dN;
		var letterStatus = model.dO;
		var chatStatus = model.cX;
		switch (msg.$) {
			case 0:
				var req = msg.a;
				if (!req.$) {
					var url = req.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.dX,
							$elm$url$Url$toString(url)));
				} else {
					var urlStr = req.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$browser$Browser$Navigation$load(urlStr));
				}
			case 1:
				var url = msg.a;
				var route = $author$project$Route$getRoute(url);
				switch (route.$) {
					case 4:
						var chatIdStr = route.a;
						return _Utils_Tuple2(
							$author$project$CoreTypes$Normal(
								_Utils_update(
									model,
									{
										cX: $author$project$Chat$OpeningWs(
											$joneshf$elm_tagged$Tagged$tag(chatIdStr)),
										ej: route
									})),
							$author$project$Main$port_InitWs(chatIdStr));
					case 2:
						var letterIdStr = route.a;
						return _Utils_Tuple2(
							$author$project$CoreTypes$Normal(
								_Utils_update(
									model,
									{ej: route})),
							$author$project$Main$getLetterReq(letterIdStr));
					case 1:
						return _Utils_Tuple2(
							$author$project$CoreTypes$Normal(
								_Utils_update(
									model,
									{
										cq: A2($author$project$Main$updateAboutPageModelWithRoute, route, model.cq),
										ej: route
									})),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(
							$author$project$CoreTypes$Normal(
								_Utils_update(
									model,
									{ej: route})),
							$elm$core$Platform$Cmd$none);
				}
			case 2:
				var viewport = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{fe: viewport})),
					$elm$core$Platform$Cmd$none);
			case 3:
				var aboutPageMsg = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{
								cq: A2($author$project$Views$About$update, aboutPageMsg, model.cq)
							})),
					$elm$core$Platform$Cmd$none);
			case 6:
				var result = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{
								dO: _Utils_update(
									letterStatus,
									{
										ed: $author$project$Letter$Got(result)
									})
							})),
					$elm$core$Platform$Cmd$none);
			case 4:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{dH: str})),
					$elm$core$Platform$Cmd$none);
			case 5:
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(model),
					A2(
						$elm$browser$Browser$Navigation$pushUrl,
						model.dX,
						$author$project$Common$Urls$frontendChatUrl(model.dH)));
			case 7:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{
								dN: _Utils_update(
									letterRawInput,
									{aa: str})
							})),
					$elm$core$Platform$Cmd$none);
			case 8:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{
								dN: _Utils_update(
									letterRawInput,
									{am: str})
							})),
					$elm$core$Platform$Cmd$none);
			case 9:
				var input = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{dM: input})),
					$elm$core$Platform$Cmd$none);
			case 10:
				var _v3 = $author$project$Letter$validateInput(letterRawInput);
				if (_v3.$ === 1) {
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$core$Platform$Cmd$none);
				} else {
					var goodInput = _v3.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{
									dN: _Utils_update(
										letterRawInput,
										{aa: '', am: '1'}),
									dO: _Utils_update(
										letterStatus,
										{
											fh: $author$project$Letter$Sent(
												{am: goodInput.am})
										})
								})),
						$elm$http$Http$request(
							{
								aa: $elm$http$Http$jsonBody(
									$elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'body',
												$elm$json$Json$Encode$string(goodInput.aa)),
												_Utils_Tuple2(
												'maxReadCount',
												$elm$json$Json$Encode$int(goodInput.am)),
												_Utils_Tuple2(
												'persist',
												$elm$json$Json$Encode$bool(model.dM))
											]))),
								aF: $elm$http$Http$expectString($author$project$CoreTypes$GotLetterSendResp),
								aY: _List_Nil,
								a3: 'PUT',
								be: $elm$core$Maybe$Nothing,
								bf: $elm$core$Maybe$Nothing,
								cj: $author$project$Common$Urls$backendWriteLetterUrl
							}));
				}
			case 11:
				var result = msg.a;
				var _v4 = model.dO.fh;
				if (_v4.$ === 1) {
					var info = _v4.a;
					var newModel = function () {
						if (result.$ === 1) {
							var err = result.a;
							return $author$project$CoreTypes$Normal(
								_Utils_update(
									model,
									{
										dO: _Utils_update(
											letterStatus,
											{
												fh: $author$project$Letter$GotResp(
													$elm$core$Result$Err(err))
											})
									}));
						} else {
							var letterId = result.a;
							return $author$project$CoreTypes$Normal(
								_Utils_update(
									model,
									{
										dO: _Utils_update(
											letterStatus,
											{
												fh: $author$project$Letter$GotResp(
													$elm$core$Result$Ok(
														{dq: letterId, am: info.am}))
											})
									}));
						}
					}();
					return _Utils_Tuple2(newModel, $elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$core$Platform$Cmd$none);
				}
			case 12:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{c9: str})),
					$elm$core$Platform$Cmd$none);
			case 13:
				var _v6 = $author$project$Utils$Types$strToPosIntInput(model.c9);
				if (_v6.$ === 1) {
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$core$Platform$Cmd$none);
				} else {
					var posInt = _v6.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{ey: $author$project$CoreTypes$Waiting_Disp})),
						$elm$http$Http$request(
							{
								aa: A2(
									$elm$http$Http$stringBody,
									'text/plain;charset=utf-8',
									$elm$core$String$fromInt(posInt)),
								aF: $elm$http$Http$expectString($author$project$CoreTypes$GotSpawnDispChatResp),
								aY: _List_Nil,
								a3: 'PUT',
								be: $elm$core$Maybe$Nothing,
								bf: $elm$core$Maybe$Nothing,
								cj: $author$project$Common$Urls$backendSpawnDispChatUrl
							}));
				}
			case 14:
				var result = msg.a;
				if (result.$ === 1) {
					var err = result.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{
									ey: $author$project$CoreTypes$GotError_Disp(err)
								})),
						$elm$core$Platform$Cmd$none);
				} else {
					var chatIdStr = result.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{
									ey: $author$project$CoreTypes$GotChatId(chatIdStr)
								})),
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.dX,
							$author$project$Common$Urls$frontendChatUrl(
								$elm_community$string_extra$String$Extra$unquote(chatIdStr))));
				}
			case 15:
				var str = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{d9: str})),
					$elm$core$Platform$Cmd$none);
			case 16:
				var _v8 = $author$project$Utils$Types$strToPosIntInput(model.d9);
				if (_v8.$ === 1) {
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$core$Platform$Cmd$none);
				} else {
					var posInt = _v8.a;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{ez: $author$project$CoreTypes$Waiting_Persist})),
						$elm$http$Http$request(
							{
								aa: A2(
									$elm$http$Http$stringBody,
									'text/plain;charset=utf-8',
									$elm$core$String$fromInt(posInt)),
								aF: $elm$http$Http$expectString($author$project$CoreTypes$GotSpawnPersistChatResp),
								aY: _List_Nil,
								a3: 'PUT',
								be: $elm$core$Maybe$Nothing,
								bf: $elm$core$Maybe$Nothing,
								cj: $author$project$Common$Urls$backendSpawnPersistChatUrl
							}));
				}
			case 17:
				var result = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{
								ez: function () {
									if (result.$ === 1) {
										var err = result.a;
										return $author$project$CoreTypes$GotError_Persist(err);
									} else {
										var letterId = result.a;
										return $author$project$CoreTypes$GotLetterId(letterId);
									}
								}()
							})),
					$elm$core$Platform$Cmd$none);
			case 18:
				var chatElmMsg = msg.a;
				var _v10 = A3($author$project$Views$Chat$update, chatElmMsg, model.cX, model.ff);
				var status = _v10.a;
				var cmd = _v10.b;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{cX: status})),
					A2($elm$core$Platform$Cmd$map, $author$project$CoreTypes$ChatElmMsg, cmd));
			case 19:
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(model),
					$author$project$Main$getViewportCmd);
			case 20:
				var visibility = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{ff: visibility})),
					$elm$core$Platform$Cmd$none);
			case 21:
				var key = msg.a;
				var _v11 = model.ej;
				if (_v11.$ === 4) {
					var _v12 = A2($author$project$Views$Chat$handleKeyDown, model.cX, key);
					var status = _v12.a;
					var cmd = _v12.b;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{cX: status})),
						A2($elm$core$Platform$Cmd$map, $author$project$CoreTypes$ChatElmMsg, cmd));
				} else {
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$core$Platform$Cmd$none);
				}
			case 22:
				var key = msg.a;
				var _v13 = model.ej;
				if (_v13.$ === 4) {
					var _v14 = A2($author$project$Views$Chat$handleKeyUp, model.cX, key);
					var status = _v14.a;
					var cmd = _v14.b;
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(
							_Utils_update(
								model,
								{cX: status})),
						A2($elm$core$Platform$Cmd$map, $author$project$CoreTypes$ChatElmMsg, cmd));
				} else {
					return _Utils_Tuple2(
						$author$project$CoreTypes$Normal(model),
						$elm$core$Platform$Cmd$none);
				}
			case 23:
				var time = msg.a;
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(
						_Utils_update(
							model,
							{ce: time})),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					$author$project$CoreTypes$Normal(model),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$update = F2(
	function (msg, state) {
		if (state.$ === 1) {
			var model = state.a;
			return A2($author$project$Main$updateModel, msg, model);
		} else {
			return _Utils_Tuple2(state, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var $author$project$Common$Colors$bgColor = A3($mdgriffith$elm_ui$Element$rgb255, 20, 30, 36);
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
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
var $mdgriffith$elm_ui$Internal$Style$classes = {cr: 'a', aR: 'atv', ct: 'ab', cu: 'cx', cv: 'cy', cw: 'acb', cx: 'accx', cy: 'accy', cz: 'acr', bl: 'al', bm: 'ar', cA: 'at', aS: 'ah', aT: 'av', cE: 's', cI: 'bh', cJ: 'b', cL: 'w7', cN: 'bd', cO: 'bdt', av: 'bn', cP: 'bs', aw: 'cpe', c_: 'cp', c$: 'cpx', c0: 'cpy', I: 'c', aB: 'ctr', aC: 'cb', aD: 'ccx', J: 'ccy', aj: 'cl', aE: 'cr', c3: 'ct', c5: 'cptr', c6: 'ctxt', dg: 'fcs', bw: 'focus-within', dh: 'fs', di: 'g', aX: 'hbh', aZ: 'hc', bA: 'he', a_: 'hf', bB: 'hfp', dn: 'hv', ds: 'ic', du: 'fr', aH: 'lbl', dx: 'iml', dy: 'imlf', dz: 'imlp', dA: 'implw', dB: 'it', dG: 'i', dQ: 'lnk', ad: 'nb', bQ: 'notxt', d_: 'ol', d0: 'or', V: 'oq', d6: 'oh', bU: 'pg', d7: 'p', d8: 'ppe', ei: 'ui', ek: 'r', en: 'sb', eo: 'sbx', ep: 'sby', eq: 'sbt', eu: 'e', ev: 'cap', ew: 'sev', eF: 'sk', eR: 't', eS: 'tc', eT: 'w8', eU: 'w2', eV: 'w9', eW: 'tj', aN: 'tja', eX: 'tl', eY: 'w3', eZ: 'w5', e_: 'w4', e$: 'tr', e0: 'w6', e1: 'w1', e2: 'tun', ch: 'ts', Y: 'clr', fa: 'u', bh: 'wc', cn: 'we', bi: 'wf', co: 'wfp', bj: 'wrp'};
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var $mdgriffith$elm_ui$Internal$Model$asEl = 2;
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.I);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.di);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bU);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.d7);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ek);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.eu);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
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
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
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
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var $mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$html$Html$div = _VirtualDom_node('div');
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 3:
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
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
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
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.el)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.D)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.ex.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.ex.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.ek) + ('-' + ($elm$core$String$fromInt(pos.bq) + ('-' + ($elm$core$String$fromInt(pos.at) + ('-' + $elm$core$String$fromInt(pos.bz)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
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
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $elm$core$String$fromFloat = _String_fromNumber;
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
					shadow.bG ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.b.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.b.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow._) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.ca) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.ab))
				])));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
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
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bw) + ':focus-within',
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
						focus.cM),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.cG),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										_: shadow._,
										ab: shadow.ab,
										bG: false,
										b: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.b)),
										ca: shadow.ca
									}));
						},
						focus.es),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + ':focus .focusable, ') + (($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + '.focusable:focus, ') + ('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + ' .focusable-thumb'))),
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
						focus.cM),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.cG),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										_: shadow._,
										ab: shadow.ab,
										bG: false,
										b: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.b)),
										ca: shadow.ca
									}));
						},
						focus.es),
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
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = 3;
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = 2;
var $mdgriffith$elm_ui$Internal$Style$Self = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var $mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var $mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var $mdgriffith$elm_ui$Internal$Style$Top = 0;
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c3);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aC);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aE);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aj);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aD);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.J);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cA);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ct);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bm);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bl);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cu);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv);
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
				$mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
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
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aX),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cI),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eq),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eR),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aZ),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.co),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bh),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
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
				case 1:
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
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
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
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
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
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = 0;
var $mdgriffith$elm_ui$Internal$Style$Behind = 5;
var $mdgriffith$elm_ui$Internal$Style$Below = 1;
var $mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var $mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var $mdgriffith$elm_ui$Internal$Style$Within = 4;
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _v0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
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
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eu),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ds))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
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
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
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
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ei),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.du),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ad),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ad),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eu),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cr),
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
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
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
							case 1:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cJ),
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
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d0),
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
							case 3:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d_),
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
							case 4:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.du),
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
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cI),
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
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
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
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bj),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bQ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c5),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c6),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d8),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aw),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.Y),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.V),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.dn, $mdgriffith$elm_ui$Internal$Style$classes.Y)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.dn, $mdgriffith$elm_ui$Internal$Style$classes.V)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.dg, $mdgriffith$elm_ui$Internal$Style$classes.Y)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.dg, $mdgriffith$elm_ui$Internal$Style$classes.V)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.aR, $mdgriffith$elm_ui$Internal$Style$classes.Y)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.aR, $mdgriffith$elm_ui$Internal$Style$classes.V)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ch),
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
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.en),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eo),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ek),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ep),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.I),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eu),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c_),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c$),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c0),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bh),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.av),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cN),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cO),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cP),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eR),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dB),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'inherit')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eu),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ek),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cn),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dQ),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bB),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aB),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cz,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cx,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cu),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cx,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cu),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cx,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cx + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cz + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.cx)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
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
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ew),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aH),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'baseline')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.I),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0px'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', 'min-content'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bA),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a_),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.co),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bh),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cw,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cy,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cy,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.cy,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cy + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.cw + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.cy)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
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
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aB),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ew),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.di),
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
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE),
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
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
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
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bU),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.cE + ($mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.cE))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.cE + ($mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.cE))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
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
								case 3:
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
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dx),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dA),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eu),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dy),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d7),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-wrap', 'break-word'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aX),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cI),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eR),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d7),
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
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eu),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cn),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.du),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cI),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cr),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cJ),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d0),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d_),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eR),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ek),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.I),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.di),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
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
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e1),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eU),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e_),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eZ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cL),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eT),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eV),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dG),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eF),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fa),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fa),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eF)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e2),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eW),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aN),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eS),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e$),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eX),
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
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.cE + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.cE + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ek) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ek) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aB) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {aA: closing, q: _List_Nil, N: _List_Nil, B: selector};
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								N: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.N)
							});
					case 3:
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									{aA: '\n}', q: _List_Nil, N: props, B: '@supports (' + (prop + (':' + (value + (') {' + parent.B))))},
									rendered.q)
							});
					case 5:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.B + (' + ' + selector), ''),
										adjRules),
									rendered.q)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.B + (' > ' + child), ''),
										childRules),
									rendered.q)
							});
					case 2:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.B + (' ' + child), ''),
										childRules),
									rendered.q)
							});
					case 4:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.B, descriptor),
											''),
										descriptorRules),
									rendered.q)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.B, ''),
										batched),
									rendered.q)
							});
				}
			});
		return A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
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
		var _v2 = rule.N;
		if (!_v2.b) {
			return '';
		} else {
			return rule.B + ('{' + (renderValues(rule.N) + (rule.aA + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.q)));
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
	var _v0 = opts.dS;
	switch (_v0) {
		case 0:
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
		case 1:
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
				_Json_emptyArray(0),
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
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.bP;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.cl);
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
		if (maybePseudo.$ === 1) {
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
			switch (pseudo) {
				case 1:
					var _v2 = options.dn;
					switch (_v2) {
						case 0:
							return _List_Nil;
						case 2:
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
				case 0:
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[
							selector + ('-fs:focus {' + (renderedProps + '\n}')),
							('.' + ($mdgriffith$elm_ui$Internal$Style$classes.cE + (':focus ' + (selector + '-fs  {')))) + (renderedProps + '\n}'),
							(selector + '-fs:focus-within {') + (renderedProps + '\n}'),
							('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cE) + (' .focusable-thumb' + (selector + '-fs {')))) + (renderedProps + '\n}')
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
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.cl)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
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
			case 0:
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 13:
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
			case 12:
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
			case 2:
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
			case 1:
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
			case 3:
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
			case 4:
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
			case 5:
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.eu;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.ek;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.bj + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bm;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.d7;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bU;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bl;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.I;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cE;
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
			case 7:
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
			case 6:
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
			case 8:
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 0:
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 1:
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 1) {
										if (_v2.b.$ === 1) {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 1) {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 2:
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 1) {
										if (_v7.b.$ === 1) {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 1) {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 3:
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
				var xSpacing = toGridLength(template.ex.a);
				var ySpacing = toGridLength(template.ex.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.el)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.D)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.D)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.ex.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.ex.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.D)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.el)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.D)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.ex.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.ex.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 9:
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.ek) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.bz) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.bq) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.at) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.ek) + (' / ' + ($elm$core$String$fromInt(position.ek + position.bz) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.bq) + (' / ' + ($elm$core$String$fromInt(position.bq + position.at) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.ek) + ('-' + ($elm$core$String$fromInt(position.bq) + ('-' + ($elm$core$String$fromInt(position.at) + ('-' + $elm$core$String$fromInt(position.bz)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 11:
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
				if ((!_v12.a.$) && (!_v12.b.$)) {
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
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.eR + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.eR)))))))))), textAdjustment)
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
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.ev, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.dh, full)));
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
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.ev))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.eR + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.ev + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.eR)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {bz: height / size, ca: size, cm: vertical};
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
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.cT, adjustment.cH, adjustment.c7, adjustment.dR]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.c7,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.cH,
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
		adjustment.cT,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		cT: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		by: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
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
				$elm$core$String$fromFloat(converted.bz)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.cm) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.ca) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _v2 = _with.cs;
						if (_v2.$ === 1) {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.by;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.cT;
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
		if (font.$ === 4) {
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
		if (_v0.$ === 1) {
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
	if (rule.$ === 1) {
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
					aM: _Utils_ap(
						rendered.aM,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					aq: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 1) {
							return rendered.aq;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.aq);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{aM: _List_Nil, aq: _List_Nil},
			stylesheet);
		var topLevel = _v0.aq;
		var rules = _v0.aM;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.dS;
		switch (_v0) {
			case 0:
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
			case 1:
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
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.dg)),
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
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.dg)),
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
		if (!myFlag.$) {
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
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
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
								case 0:
									return unkeyed;
								case 2:
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
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
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
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.eu))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.cE, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.aB, $mdgriffith$elm_ui$Internal$Style$classes.J, $mdgriffith$elm_ui$Internal$Style$classes.cz])))
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
									[$mdgriffith$elm_ui$Internal$Style$classes.cE, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.aB, $mdgriffith$elm_ui$Internal$Style$classes.J, $mdgriffith$elm_ui$Internal$Style$classes.cx])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.cE, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.aB, $mdgriffith$elm_ui$Internal$Style$classes.cy])))
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
									[$mdgriffith$elm_ui$Internal$Style$classes.cE, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.aB, $mdgriffith$elm_ui$Internal$Style$classes.cw])))
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
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.eR + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bh + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aZ)))));
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
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.cE + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.eR + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bi + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.a_)))));
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
					case 0:
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
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.$7, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.eI : _Utils_ap(styled.eI, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.$7, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.eI : _Utils_ap(styled.eI, existingStyles));
					case 2:
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
					case 0:
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
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.$7, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.eI : _Utils_ap(styled.eI, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.$7, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.eI : _Utils_ap(styled.eI, existingStyles));
					case 2:
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
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.eI : _Utils_ap(rendered.eI, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.S,
						rendered.U,
						rendered.P,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.cZ)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						$7: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.S,
							rendered.U,
							rendered.P,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.cZ))),
						eI: allStyles
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
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.eI : _Utils_ap(rendered.eI, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.S,
						rendered.U,
						rendered.P,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.cZ)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						$7: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.S,
							rendered.U,
							rendered.P,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.cZ))),
						eI: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.ad, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.cr]));
							case 1:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.ad, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.cJ]));
							case 2:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.ad, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.d0]));
							case 3:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.ad, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.d_]));
							case 4:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.ad, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.du]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.ad, $mdgriffith$elm_ui$Internal$Style$classes.eu, $mdgriffith$elm_ui$Internal$Style$classes.cI]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return $elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.$7, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
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
				if (location === 5) {
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
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.aS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bl);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.aS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bm);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.aS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cu);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.aT + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cA);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.aT + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ct);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.aT + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cv);
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
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
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
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
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
					case 0:
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
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
		case 0:
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.bA + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aZ,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.a_,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bB + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.cE + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.I + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
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
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.cn + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bh,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bi,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.co + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.cE + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.ek + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
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
			if (style.$ === 3) {
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
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
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
				if (_v1.$ === 1) {
					return {
						P: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						cZ: children,
						S: has,
						U: node,
						eI: styles
					};
				} else {
					var _class = _v1.a;
					return {
						P: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						cZ: children,
						S: has,
						U: node,
						eI: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
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
					case 3:
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
					case 1:
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
					case 4:
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
					case 10:
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
					case 7:
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
								case 0:
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.cn + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
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
								case 1:
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bh),
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
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bi),
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
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.co + (' width-fill-' + $elm$core$String$fromInt(portion)))),
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
												$mdgriffith$elm_ui$Internal$Style$classes.cE + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.ek + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
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
					case 8:
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
								case 0:
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.bA + (' ' + (name + (' ' + classes))),
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
								case 1:
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.aZ + (' ' + classes),
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
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.a_ + (' ' + classes),
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
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bB + (' height-fill-' + $elm$core$String$fromInt(portion)))),
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
												$mdgriffith$elm_ui$Internal$Style$classes.cE + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.I + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
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
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
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
							case 1:
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
							case 2:
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
							case 3:
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
							case 4:
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
							case 9:
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
							case 8:
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
							case 5:
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
							case 6:
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
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.eI);
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
					case 6:
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
								switch (x) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
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
								switch (y) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
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
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
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
var $mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var $mdgriffith$elm_ui$Internal$Model$Layout = 0;
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	cG: $elm$core$Maybe$Nothing,
	cM: $elm$core$Maybe$Nothing,
	es: $elm$core$Maybe$Just(
		{
			_: 0,
			ab: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			b: _Utils_Tuple2(0, 0),
			ca: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _v4 = record.dn;
					if (_v4.$ === 1) {
						return _Utils_update(
							record,
							{
								dn: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _v5 = record.dg;
					if (_v5.$ === 1) {
						return _Utils_update(
							record,
							{
								dg: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.dS;
					if (_v6.$ === 1) {
						return _Utils_update(
							record,
							{
								dS: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			dg: function () {
				var _v0 = record.dg;
				if (_v0.$ === 1) {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			dn: function () {
				var _v1 = record.dn;
				if (_v1.$ === 1) {
					return 1;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			dS: function () {
				var _v2 = record.dS;
				if (_v2.$ === 1) {
					return 0;
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
			{dg: $elm$core$Maybe$Nothing, dn: $elm$core$Maybe$Nothing, dS: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.eI;
				var html = el.a.$7;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
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
			var _v0 = options.dS;
			if (_v0 === 1) {
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
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
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
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.bP;
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
		var options = _v0.bS;
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
							[$mdgriffith$elm_ui$Internal$Style$classes.ei, $mdgriffith$elm_ui$Internal$Style$classes.cE, $mdgriffith$elm_ui$Internal$Style$classes.eu]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{bS: _List_Nil});
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $author$project$CoreTypes$AboutPageMsg = function (a) {
	return {$: 3, a: a};
};
var $author$project$CoreTypes$JoinChat = {$: 5};
var $author$project$CoreTypes$JoinChatInput = function (a) {
	return {$: 4, a: a};
};
var $author$project$Common$Urls$aboutUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['about']),
	_List_Nil);
var $mdgriffith$elm_ui$Internal$Model$Button = {$: 8};
var $mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var $mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 0};
var $mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 4) && (attr.b.$ === 11)) && (!attr.b.a)) {
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
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
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
	return {$: 2, a: a};
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
		if (_v0.$ === 1) {
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
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$cursor = $mdgriffith$elm_ui$Internal$Flag$flag(21);
var $mdgriffith$elm_ui$Element$pointer = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.c5);
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $mdgriffith$elm_ui$Element$Input$space = ' ';
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _v0) {
		var onPress = _v0.d$;
		var label = _v0.a0;
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
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aD + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.J + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.eq + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bQ)))))),
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
											if (onPress.$ === 1) {
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
var $author$project$Common$Urls$configChatUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['config-chat']),
	_List_Nil);
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
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $author$project$Common$Urls$frontendWriteLetterUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['write-letter']),
	_List_Nil);
var $mdgriffith$elm_ui$Element$Input$HiddenLabel = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Element$Input$labelHidden = $mdgriffith$elm_ui$Element$Input$HiddenLabel;
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $mdgriffith$elm_ui$Element$link = F2(
	function (attrs, _v0) {
		var url = _v0.cj;
		var label = _v0.a0;
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
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aD + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.J + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dQ)))),
								attrs))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_ui$Internal$Model$Empty = {$: 3};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $mdgriffith$elm_ui$Internal$Model$map = F2(
	function (fn, el) {
		switch (el.$) {
			case 1:
				var styled = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						$7: F2(
							function (add, context) {
								return A2(
									$elm$virtual_dom$VirtualDom$map,
									fn,
									A2(styled.$7, add, context));
							}),
						eI: styled.eI
					});
			case 0:
				var html = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A2(
						$elm$core$Basics$composeL,
						$elm$virtual_dom$VirtualDom$map(fn),
						html));
			case 2:
				var str = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Text(str);
			default:
				return $mdgriffith$elm_ui$Internal$Model$Empty;
		}
	});
var $mdgriffith$elm_ui$Element$map = $mdgriffith$elm_ui$Internal$Model$map;
var $mdgriffith$elm_ui$Internal$Model$Max = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$maximum = F2(
	function (i, l) {
		return A2($mdgriffith$elm_ui$Internal$Model$Max, i, l);
	});
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
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
var $mdgriffith$elm_ui$Element$Input$Placeholder = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$Input$placeholder = $mdgriffith$elm_ui$Element$Input$Placeholder;
var $mdgriffith$elm_ui$Internal$Model$Paragraph = {$: 9};
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
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
var $author$project$Common$Contents$plainPara = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$paragraph,
		_List_Nil,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$px = $mdgriffith$elm_ui$Internal$Model$Px;
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
var $mdgriffith$elm_ui$Internal$Model$AsRow = 0;
var $mdgriffith$elm_ui$Internal$Model$asRow = 0;
var $mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asRow,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aj + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.J)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
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
var $mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$Input$TextArea = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 6};
var $mdgriffith$elm_ui$Element$Region$announce = $mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$LivePolite);
var $mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var $mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var $mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 1) {
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
			switch (position) {
				case 2:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aH),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 3:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aH),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 0:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aH),
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
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aH),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $mdgriffith$elm_ui$Element$Input$autofill = A2(
	$elm$core$Basics$composeL,
	$mdgriffith$elm_ui$Internal$Model$Attr,
	$elm$html$Html$Attributes$attribute('autocomplete'));
var $mdgriffith$elm_ui$Internal$Model$Behind = 5;
var $mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$createNearby = F2(
	function (loc, element) {
		if (element.$ === 3) {
			return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
		} else {
			return A2($mdgriffith$elm_ui$Internal$Model$Nearby, loc, element);
		}
	});
var $mdgriffith$elm_ui$Element$behindContent = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 5, element);
};
var $mdgriffith$elm_ui$Internal$Model$MoveY = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$moveY = $mdgriffith$elm_ui$Internal$Flag$flag(26);
var $mdgriffith$elm_ui$Element$moveUp = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(-y));
};
var $mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding = function (attrs) {
	var gatherSpacing = F2(
		function (attr, found) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v2 = attr.b;
				var x = _v2.b;
				var y = _v2.c;
				if (found.$ === 1) {
					return $elm$core$Maybe$Just(y);
				} else {
					return found;
				}
			} else {
				return found;
			}
		});
	var _v0 = A3($elm$core$List$foldr, gatherSpacing, $elm$core$Maybe$Nothing, attrs);
	if (_v0.$ === 1) {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	} else {
		var vSpace = _v0.a;
		return $mdgriffith$elm_ui$Element$moveUp(
			$elm$core$Basics$floor(vSpace / 2));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$overflow = $mdgriffith$elm_ui$Internal$Flag$flag(20);
var $mdgriffith$elm_ui$Element$clip = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.c_);
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
var $mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var $mdgriffith$elm_ui$Element$Input$darkGrey = A3($mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var $mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2($mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var $mdgriffith$elm_ui$Element$Input$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 6, a: a, b: b, c: c, d: d, e: e};
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
	if (attr.$ === 8) {
		var h = attr.a;
		return $elm$core$Maybe$Just(h);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 1) {
		var textLabel = label.a;
		return $mdgriffith$elm_ui$Internal$Model$Describe(
			$mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var $mdgriffith$elm_ui$Internal$Model$InFront = 4;
var $mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 4, element);
};
var $mdgriffith$elm_ui$Element$Input$isConstrained = function (len) {
	isConstrained:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return true;
			case 3:
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
	if (label.$ === 1) {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$isStacked = function (label) {
	if (!label.$) {
		var loc = label.a;
		switch (loc) {
			case 0:
				return false;
			case 1:
				return false;
			case 2:
				return true;
			default:
				return true;
		}
	} else {
		return true;
	}
};
var $mdgriffith$elm_ui$Element$Input$negateBox = function (box) {
	return {cQ: -box.cQ, dK: -box.dK, eh: -box.eh, e5: -box.e5};
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
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
var $mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left)))))));
	});
var $mdgriffith$elm_ui$Element$paddingEach = function (_v0) {
	var top = _v0.e5;
	var right = _v0.eh;
	var bottom = _v0.cQ;
	var left = _v0.dK;
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
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $mdgriffith$elm_ui$Element$Input$isFill = function (len) {
	isFill:
	while (true) {
		switch (len.$) {
			case 2:
				return true;
			case 1:
				return false;
			case 0:
				return false;
			case 3:
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
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return false;
			case 3:
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
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $mdgriffith$elm_ui$Element$Input$redistributeOver = F4(
	function (isMultiline, stacked, attr, els) {
		switch (attr.$) {
			case 9:
				return _Utils_update(
					els,
					{
						d: A2($elm$core$List$cons, attr, els.d)
					});
			case 7:
				var width = attr.a;
				return $mdgriffith$elm_ui$Element$Input$isFill(width) ? _Utils_update(
					els,
					{
						i: A2($elm$core$List$cons, attr, els.i),
						dw: A2($elm$core$List$cons, attr, els.dw),
						d: A2($elm$core$List$cons, attr, els.d)
					}) : (stacked ? _Utils_update(
					els,
					{
						i: A2($elm$core$List$cons, attr, els.i)
					}) : _Utils_update(
					els,
					{
						d: A2($elm$core$List$cons, attr, els.d)
					}));
			case 8:
				var height = attr.a;
				return (!stacked) ? _Utils_update(
					els,
					{
						i: A2($elm$core$List$cons, attr, els.i),
						d: A2($elm$core$List$cons, attr, els.d)
					}) : ($mdgriffith$elm_ui$Element$Input$isFill(height) ? _Utils_update(
					els,
					{
						i: A2($elm$core$List$cons, attr, els.i),
						d: A2($elm$core$List$cons, attr, els.d)
					}) : ($mdgriffith$elm_ui$Element$Input$isPixel(height) ? _Utils_update(
					els,
					{
						d: A2($elm$core$List$cons, attr, els.d)
					}) : _Utils_update(
					els,
					{
						d: A2($elm$core$List$cons, attr, els.d)
					})));
			case 6:
				return _Utils_update(
					els,
					{
						i: A2($elm$core$List$cons, attr, els.i)
					});
			case 5:
				return _Utils_update(
					els,
					{
						i: A2($elm$core$List$cons, attr, els.i)
					});
			case 4:
				switch (attr.b.$) {
					case 5:
						var _v1 = attr.b;
						return _Utils_update(
							els,
							{
								i: A2($elm$core$List$cons, attr, els.i),
								dw: A2($elm$core$List$cons, attr, els.dw),
								d: A2($elm$core$List$cons, attr, els.d),
								ah: A2($elm$core$List$cons, attr, els.ah)
							});
					case 7:
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
									u: A2($elm$core$List$cons, attr, els.u),
									d: A2($elm$core$List$cons, attr, els.d)
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
									u: A2($elm$core$List$cons, attr, els.u),
									dw: A2(
										$elm$core$List$cons,
										newHeight,
										A2($elm$core$List$cons, newLineHeight, els.dw)),
									d: A2($elm$core$List$cons, reducedVerticalPadding, els.d)
								});
						}
					case 6:
						var _v3 = attr.b;
						return _Utils_update(
							els,
							{
								u: A2($elm$core$List$cons, attr, els.u),
								d: A2($elm$core$List$cons, attr, els.d)
							});
					case 10:
						return _Utils_update(
							els,
							{
								u: A2($elm$core$List$cons, attr, els.u),
								d: A2($elm$core$List$cons, attr, els.d)
							});
					case 2:
						return _Utils_update(
							els,
							{
								i: A2($elm$core$List$cons, attr, els.i)
							});
					case 1:
						var _v4 = attr.b;
						return _Utils_update(
							els,
							{
								i: A2($elm$core$List$cons, attr, els.i)
							});
					default:
						var flag = attr.a;
						var cls = attr.b;
						return _Utils_update(
							els,
							{
								d: A2($elm$core$List$cons, attr, els.d)
							});
				}
			case 0:
				return els;
			case 1:
				var a = attr.a;
				return _Utils_update(
					els,
					{
						dw: A2($elm$core$List$cons, attr, els.dw)
					});
			case 2:
				return _Utils_update(
					els,
					{
						dw: A2($elm$core$List$cons, attr, els.dw)
					});
			case 3:
				return _Utils_update(
					els,
					{
						d: A2($elm$core$List$cons, attr, els.d)
					});
			default:
				return _Utils_update(
					els,
					{
						dw: A2($elm$core$List$cons, attr, els.dw)
					});
		}
	});
var $mdgriffith$elm_ui$Element$Input$redistribute = F3(
	function (isMultiline, stacked, attrs) {
		return function (redist) {
			return {
				u: $elm$core$List$reverse(redist.u),
				i: $elm$core$List$reverse(redist.i),
				dw: $elm$core$List$reverse(redist.dw),
				d: $elm$core$List$reverse(redist.d),
				ah: $elm$core$List$reverse(redist.ah)
			};
		}(
			A3(
				$elm$core$List$foldl,
				A2($mdgriffith$elm_ui$Element$Input$redistributeOver, isMultiline, stacked),
				{u: _List_Nil, i: _List_Nil, dw: _List_Nil, d: _List_Nil, ah: _List_Nil},
				attrs));
	});
var $mdgriffith$elm_ui$Element$Input$renderBox = function (_v0) {
	var top = _v0.e5;
	var right = _v0.eh;
	var bottom = _v0.cQ;
	var left = _v0.dK;
	return $elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px'))))));
};
var $mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$transparency = $mdgriffith$elm_ui$Internal$Flag$flag(0);
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
var $mdgriffith$elm_ui$Element$rgba = $mdgriffith$elm_ui$Internal$Model$Rgba;
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
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bQ + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.d8)),
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
var $mdgriffith$elm_ui$Element$scrollbarY = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.ep);
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $mdgriffith$elm_ui$Element$Input$spellcheck = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$spellcheck);
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $mdgriffith$elm_ui$Element$Input$value = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$value);
var $mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var withDefaults = _Utils_ap($mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var redistributed = A3(
			$mdgriffith$elm_ui$Element$Input$redistribute,
			_Utils_eq(textInput.j, $mdgriffith$elm_ui$Element$Input$TextArea),
			$mdgriffith$elm_ui$Element$Input$isStacked(textOptions.a0),
			withDefaults);
		var onlySpacing = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v9 = attr.b;
				return true;
			} else {
				return false;
			}
		};
		var heightConstrained = function () {
			var _v7 = textInput.j;
			if (!_v7.$) {
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
			if ((attr.$ === 4) && (attr.b.$ === 7)) {
				var cls = attr.a;
				var _v6 = attr.b;
				var pad = _v6.a;
				var t = _v6.b;
				var r = _v6.c;
				var b = _v6.d;
				var l = _v6.e;
				return $elm$core$Maybe$Just(
					{
						cQ: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(b - 3)),
						dK: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(l - 3)),
						eh: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(r - 3)),
						e5: A2(
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
			{cQ: 0, dK: 0, eh: 0, e5: 0},
			$elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$List$filterMap, getPadding, withDefaults))));
		var inputElement = A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			function () {
				var _v3 = textInput.j;
				if (!_v3.$) {
					var inputType = _v3.a;
					return $mdgriffith$elm_ui$Internal$Model$NodeName('input');
				} else {
					return $mdgriffith$elm_ui$Internal$Model$NodeName('textarea');
				}
			}(),
			_Utils_ap(
				function () {
					var _v4 = textInput.j;
					if (!_v4.$) {
						var inputType = _v4.a;
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Internal$Model$Attr(
								$elm$html$Html$Attributes$type_(inputType)),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.dB)
							]);
					} else {
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Element$clip,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.dx),
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
							$mdgriffith$elm_ui$Element$Input$value(textOptions.eR),
							$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Events$onInput(textOptions.dZ)),
							$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions.a0),
							$mdgriffith$elm_ui$Element$Input$spellcheck(textInput.C),
							A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Internal$Model$NoAttribute,
							A2($elm$core$Maybe$map, $mdgriffith$elm_ui$Element$Input$autofill, textInput.y))
						]),
					redistributed.dw)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil));
		var wrappedInput = function () {
			var _v0 = textInput.j;
			if (_v0.$ === 1) {
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					_Utils_ap(
						(heightConstrained ? $elm$core$List$cons($mdgriffith$elm_ui$Element$scrollbarY) : $elm$core$Basics$identity)(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bw),
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.dA)
								])),
						redistributed.d),
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
												$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.dz),
												redistributed.ah)))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(
									function () {
										if (textOptions.eR === '') {
											var _v1 = textOptions.ea;
											if (_v1.$ === 1) {
												return _List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('\u00A0')
													]);
											} else {
												var place = _v1.a;
												return _List_fromArray(
													[
														A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, _List_Nil, textOptions.eR === '')
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
																$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.dy)
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(textOptions.eR + '\u00A0')
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
							A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bw),
							$elm$core$List$concat(
								_List_fromArray(
									[
										redistributed.d,
										function () {
										var _v2 = textOptions.ea;
										if (_v2.$ === 1) {
											return _List_Nil;
										} else {
											var place = _v2.a;
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$behindContent(
													A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, redistributed.u, textOptions.eR === ''))
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
				A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.c6),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions.a0) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(5),
					A2($elm$core$List$cons, $mdgriffith$elm_ui$Element$Region$announce, redistributed.i))),
			textOptions.a0,
			wrappedInput);
	});
var $mdgriffith$elm_ui$Element$Input$text = $mdgriffith$elm_ui$Element$Input$textHelper(
	{
		y: $elm$core$Maybe$Nothing,
		C: false,
		j: $mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var $mdgriffith$elm_ui$Internal$Model$AsTextColumn = 5;
var $mdgriffith$elm_ui$Internal$Model$asTextColumn = 5;
var $mdgriffith$elm_ui$Internal$Model$Min = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
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
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.c3 + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aj)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $author$project$Views$About$Body = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Element$Font$italic = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.dG);
var $author$project$Common$Contents$italicText = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[$mdgriffith$elm_ui$Element$Font$italic]),
		$mdgriffith$elm_ui$Element$text(str));
};
var $author$project$Views$About$OnCollapseSection = {$: 1};
var $author$project$Views$About$OnExpandSection = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$none = $mdgriffith$elm_ui$Internal$Model$Empty;
var $mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontSize,
		$mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var $author$project$Views$About$titleFontStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Font$size(32)
	]);
var $author$project$Views$About$mkSection = F4(
	function (section, titleStr, _v0, model) {
		var body = _v0;
		var title = A2(
			$mdgriffith$elm_ui$Element$Input$button,
			$author$project$Views$About$titleFontStyle,
			{
				a0: $author$project$Common$Contents$plainPara(titleStr),
				d$: $elm$core$Maybe$Just(
					_Utils_eq(section, model.b7) ? $author$project$Views$About$OnCollapseSection : $author$project$Views$About$OnExpandSection(section))
			});
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20)
				]),
			_List_fromArray(
				[
					title,
					_Utils_eq(section, model.b7) ? body : $mdgriffith$elm_ui$Element$none
				]));
	});
var $mdgriffith$elm_ui$Element$Font$underline = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.fa);
var $author$project$Common$Styles$linkStyle = _List_fromArray(
	[$mdgriffith$elm_ui$Element$Font$underline]);
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $mdgriffith$elm_ui$Element$newTabLink = F2(
	function (attrs, _v0) {
		var url = _v0.cj;
		var label = _v0.a0;
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
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aD + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.J + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dQ)))),
									attrs)))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $author$project$Common$Contents$newTabLink = F2(
	function (urlStr, labelStr) {
		return A2(
			$mdgriffith$elm_ui$Element$newTabLink,
			$author$project$Common$Styles$linkStyle,
			{
				a0: $mdgriffith$elm_ui$Element$text(labelStr),
				cj: urlStr
			});
	});
var $author$project$Views$About$paraSpacing = A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20);
var $author$project$Views$About$func_prog = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_Nil,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('\n                        Hideout is built with 2 functional programming languages: Haskell and Elm. Functional programming is a programming paradigm that deserves a lot more attention and \n                        '),
						$author$project$Common$Contents$italicText('should'),
						$mdgriffith$elm_ui$Element$text('\n                         be the future. In brief, it encourages separation of side-effects, and type safety, which eliminates classes of bugs, while making refactoring and testing a charm, instead of a pain. It\'s also the first paradigm where I find myself actively seeking to learn more, because of its deep yet playful nature. The ultimate goal of functional programming is \"If it compiles, it works.\" So if you prefer to solve compile errors, rather than wrestle with runtime bugs, you\'ll like functional programming.\n                        ')
					])),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_Nil,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('I found '),
						A2($author$project$Common$Contents$newTabLink, 'http://learnyouahaskell.com/chapters', 'Learn You a Haskell for Great Good'),
						$mdgriffith$elm_ui$Element$text('\n                         to be a great tutorial to get into functional programming with Haskell. Too bad it\'s non-HTTPS, so it\'s insecure to buy a copy with credit card. You can still read it for free though. Alternatively, \n                        '),
						A2($author$project$Common$Contents$newTabLink, 'https://guide.elm-lang.org/', 'Elm'),
						$mdgriffith$elm_ui$Element$text('\n                         is famous for being easy to learn.\n                        ')
					]))
			]));
	return A4($author$project$Views$About$mkSection, 9, 'Functional Programming', body, model);
};
var $author$project$Common$Contents$underlinedText = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[$mdgriffith$elm_ui$Element$Font$underline]),
		$mdgriffith$elm_ui$Element$text(str));
};
var $author$project$Views$About$hideout_vs_apps = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[$author$project$Views$About$paraSpacing]),
				_List_fromArray(
					[
						$author$project$Common$Contents$plainPara('The project of Hideout arose from personal needs.'),
						A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20)
							]),
						_List_fromArray(
							[
								$author$project$Common$Contents$plainPara('\n                            I\'ve been recommending my family and friends to use secure messaging apps like Signal, Wire, Element and so on. It wasn\'t easy. But some of them did sign up, installed the software, and started using it. But after a while, I noticed that the graph of my contacts on these apps form a star shape, where I\'m the center. I have a lot of contacts on my end. But my friends don\'t have the initiative to further recommend those apps to their friends. I\'m the only contact on their app for each of my friend.\n                            '),
								$author$project$Common$Contents$plainPara('\n                            It\'s getting extremely fast and simple to install a new app, sign up, and start chatting nowadays. But it\'s still too much efforts for some. It probably won\'t last too long for my friends, who installed an app and has only me on the contact list. And for the other friends who aren\'t using secure messaging apps, we are still talking about personal stuff on platforms that don\'t repsect user privacy.\n                            '),
								A2(
								$mdgriffith$elm_ui$Element$paragraph,
								_List_Nil,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$text('\n                                That\'s when I started to work on Hideout. A chat service that requires no sign up, and no installation. You can just bookmark the website, start a disposable chat, and give the link to your contacts, over \n                                '),
										$author$project$Common$Contents$underlinedText('any'),
										$mdgriffith$elm_ui$Element$text('\n                                 platform, be it Gmail, Facebook, Snapchat, Discord... Or you can create and bookmark a persistent chat room, and message your contacts from your browser at any time. Hideout offers the guarantee that nobody else can spy on your conversation.\n                                ')
									]))
							]))
					]))
			]));
	return A4($author$project$Views$About$mkSection, 6, 'Why use Hideout, when there are so many secure messaging apps?', body, model);
};
var $author$project$Views$About$how_private = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('\n                    - Hideout is designed to be self-hosted. The idea is that the more privacy-aware and tech-savvy person among a friend group will host the server, to use with their friends. So trust is already a given. The user trusts the server as much as they trust the person hosting.\n                    '),
				$author$project$Common$Contents$plainPara('\n                    - Disposable letters can\'t be read after a max read limit is reached. A disposable chat room can\'t be joined after the max join limit is reached. This "access-based" approach gives a stronger guarantee of privacy than the "time-based" approach. If a message or chat room is set to be deleted after 15 minutes, nothing stops it from being viewed by unwanted parties at the 14th minute.\n                    ')
			]));
	return A4($author$project$Views$About$mkSection, 3, 'How is Hideout private?', body, model);
};
var $author$project$Common$Contents$link = F2(
	function (urlStr, labelStr) {
		return A2(
			$mdgriffith$elm_ui$Element$link,
			$author$project$Common$Styles$linkStyle,
			{
				a0: $mdgriffith$elm_ui$Element$text(labelStr),
				cj: urlStr
			});
	});
var $author$project$Views$About$persist_chat = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('\n                    Persistent chat rooms is a simple yet powerful idea. It\'s a private chat room that doesn\'t expire, so the participants can keep going back to it, without having to create a new room every time they talk. Hideout achieves this in a very simple way.\n                    '),
				$author$project$Common$Contents$plainPara('\n                    Imagine you have already created a disposable chat room and you are chatting with your friends. Everything you say in this room is private. If you go ahead and create a second chat room, you can share the link of this new room privately with your friends. Since only you and your friends know about this new room, it doesn\'t have to expire. Hideout just automates this process.\n                    '),
				$author$project$Common$Contents$plainPara('\n                    Here are the details of how it\'s automated. A person creates a persistent chat room, and sets the number of participants to 4, for example. This makes Hideout generate a chat room that can be joined infinitely on the server. Then, Hideout generates a disposable letter, which contains the room ID. The disposable letter can only be accessed 4 times. The person shares the link to this letter (not the chat!) to their 3 friends. The 4 of them each opens the letter, retrieves the room ID, and uses it to join the chat on Hideout\'s home page. Hideout deletes the letter after all 4 people have read it. So nothing else can get the room ID. But the 4 people now have a persistent chat room that they can always go back into.\n                    '),
				$author$project$Common$Contents$plainPara('\n                    Persistent chat is of great value for people who are in situations where it\'s improper to repeatedly create and share disposable chat room links, as doing so draws unwanted attention. Sure, you can create and send 1000 room links on Discord in the United States per day. But that\'s not the same in a lot of other places on the planet.\n                    ')
			]));
	return A4($author$project$Views$About$mkSection, 4, 'Persistent Chat: Where Hideout truly shines!', body, model);
};
var $author$project$Common$Urls$rootUrl = A2($elm$url$Url$Builder$absolute, _List_Nil, _List_Nil);
var $author$project$Views$About$sectionSpacing = A2($mdgriffith$elm_ui$Element$spacingXY, 0, 60);
var $author$project$Views$About$self_hosting = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('\n                    Hideout is designed to be self-hosted. The idea is that the more privacy-minded and tech-savvy person among a friend group can set up a Hideout server, for them and their friends to use. The server is naturally trusted, as the owner is a friend.\n                    '),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_Nil,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('\n                        However, \"self-hosting\" has a more strict meaning here. Ideally, the server should be run on a device that the person \n                        '),
						$author$project$Common$Contents$underlinedText('physically owns'),
						$mdgriffith$elm_ui$Element$text('\n                        . To protect the server operator\'s IP address, the server should be run behind a VPN. I\'ve experimented and confirmed that it\'s very practical. Mullvad VPN has an open-source VPN client, and offers the ability of port-forwarding. The Hideout server I ran ended up having a URL followed by a port number, like https://www.myhideout.com:12345. Not a big deal since the URL can be bookmarked in the browser. Currently the only inconvenience is that Mullvad\'s port-forwarding is under development. I couldn\'t get more than 1 port forwarded. So I couldn\'t really SSH into my server etc.\n                        ')
					])),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_Nil,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('\n                        A less ideal option is to rent a server from a VPS provider that\'s reputable for respecting user\'s privacy. Currently I can only think of \n                        '),
						A2($author$project$Common$Contents$newTabLink, 'https://njal.la', 'Njalla'),
						$mdgriffith$elm_ui$Element$text('. But I\'m open to suggestions on that.')
					])),
				$author$project$Common$Contents$plainPara('\n                    The least ideal, almost unacceptable option is to rent a server from a VPS provider that doesn\'t necessarily respect user\'s privacy, like Google Cloud, Amazon AWS, and so on. If you can rent a server there, I don\'t see why you can\'t rent a server from Njalla.\n                    '),
				$author$project$Common$Contents$plainPara('\n                    Hideout isn\'t designed to be hosted on a server that gives service to a large population. People shouldn\'t place much trust on servers run by strangers. Futhermore, such a server will probably be spammed a lot, as Hideout does not have account registration.\n                    ')
			]));
	return A4($author$project$Views$About$mkSection, 8, 'Hideout is designed to be self-hosted!', body, model);
};
var $author$project$Views$About$threat_model = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('\n                    Hideout assumes your communication on the unprivate service isn\'t compromised. If you want a private conversation without it being logged by the unprivate service, Hideout can help. If your communication on the unprivate service is being actively monitored and tampered with, you\'ll need to establish a new private communication channel with your friends, preferably in person.\n                    ')
			]));
	return A4($author$project$Views$About$mkSection, 2, 'Threat Model', body, model);
};
var $author$project$Common$Contents$sizedPara = F2(
	function (fontSize, str) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Font$size(fontSize)
				]),
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$text(str)
				]));
	});
var $author$project$Views$About$troubleshooting = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara(' \n                    If you received a link to a Hideout letter or chat, but it tells you that the maximum number of time it can be accessed is reached, then maybe some of the intended participants reloaded the letter page or rejoined the chat. If it\'s made certain that nobody is accessing multiple times, then the grim reality is probably that the communication among your friends is being spied on, and you all should move to secure messaging apps*.\n                    '),
				A2($author$project$Common$Contents$sizedPara, 16, '\n                    * Actually, it doesn\'t necessarily mean that it\'s the unprivate apps like Facebook Messenger and Discord who\'s the spying adversary. Unprivate operating systems like Windows 10 can be the spying one too. Another possibility is that someone\'s computer is hacked, and moving to a secure messaging app only obscures the hack. Overall, one should follow good security and privacy practices.\n                    ')
			]));
	return A4($author$project$Views$About$mkSection, 5, 'Troubleshooting', body, model);
};
var $author$project$Common$Colors$green = A3($mdgriffith$elm_ui$Element$rgb255, 100, 200, 0);
var $elm$url$Url$Builder$Relative = {$: 1};
var $elm$url$Url$Builder$rootToPrePath = function (root) {
	switch (root.$) {
		case 0:
			return '/';
		case 1:
			return '';
		default:
			var prePath = root.a;
			return prePath + '/';
	}
};
var $elm$url$Url$Builder$custom = F4(
	function (root, pathSegments, parameters, maybeFragment) {
		var fragmentless = _Utils_ap(
			$elm$url$Url$Builder$rootToPrePath(root),
			_Utils_ap(
				A2($elm$core$String$join, '/', pathSegments),
				$elm$url$Url$Builder$toQuery(parameters)));
		if (maybeFragment.$ === 1) {
			return fragmentless;
		} else {
			var fragment = maybeFragment.a;
			return fragmentless + ('#' + fragment);
		}
	});
var $pzp1997$assoc_list$AssocList$toList = function (_v0) {
	var alist = _v0;
	return alist;
};
var $author$project$Views$About$sectionToUrl = function (section) {
	var urlFrag = A2(
		$elm$core$Maybe$withDefault,
		'[Error: Section missing]',
		A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$first,
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (_v0) {
						var section_ = _v0.b;
						return _Utils_eq(section, section_);
					},
					$pzp1997$assoc_list$AssocList$toList($author$project$Views$About$urlFragAndSectionAssoc)))));
	return A4(
		$elm$url$Url$Builder$custom,
		$elm$url$Url$Builder$Relative,
		_List_fromArray(
			[$author$project$Common$Urls$aboutUrl]),
		_List_Nil,
		$elm$core$Maybe$Just(urlFrag));
};
var $author$project$Views$About$use_cases = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('\n                    1. Hideout is useful when you want to have a private conversation with your friends, but you are currently using a service that violates user\'s privacy, e.g. Facebook Messenger, Discord, Gmail, and so on. By using Hideout, the conversation is simply moved away from the unprivate service, leaving it nothing to collect and spy on.\n                    '),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$green)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('\n                        2. Hideout is particularly useful if you want a chat room that\'s persistent, yet still private, which you and your friends can bookmark and keep going back to. Learn more \n                        '),
						A2(
						$author$project$Common$Contents$link,
						$author$project$Views$About$sectionToUrl(4),
						'here'),
						$mdgriffith$elm_ui$Element$text('.')
					])),
				$author$project$Common$Contents$plainPara('\n                    3. Hideout is useful if your friends are scattered across multiple messaging apps. Someone on Snapchat can\'t talk to someone on Signal. But they can convene in a Hideout chat room.\n                    ')
			]));
	return A4($author$project$Views$About$mkSection, 1, 'Use Cases', body, model);
};
var $author$project$Views$About$lineSpacing = A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10);
var $author$project$Views$About$why_another_disp = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[$author$project$Views$About$paraSpacing]),
		_List_fromArray(
			[
				$author$project$Common$Contents$plainPara('\n                    Disposable chat is not a new idea. But the concept is in need of a new, good enough implementation. Before I made Hideout, I first looked up on search engines to find a disposable chat service that can serve my needs. But none satisfied me. Several aspects that I see are lacking:\n                    '),
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$author$project$Views$About$lineSpacing,
						$mdgriffith$elm_ui$Element$paddingEach(
						{cQ: 0, dK: 40, eh: 0, e5: 0})
					]),
				_List_fromArray(
					[
						$author$project$Common$Contents$plainPara('\n                        1. Some services aren\'t open source. It\'s just a website run by someone I don\'t know. Why should I trust them?\n                        '),
						$author$project$Common$Contents$plainPara('\n                        2. Some services aren\'t served over HTTPS. Even a script kiddie can spy on, or even modify, my conversations.\n                        '),
						$author$project$Common$Contents$plainPara('\n                        3. Some services don\'t have many features. Hideout plans to implement emojis, file sharing, and voice/video chats. It plans to be as feature-rich as possible.\n                        '),
						A2(
						$mdgriffith$elm_ui$Element$paragraph,
						_List_Nil,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$text('\n                            4. Some services are "time-based". A spying adversary can easily view the messages before it expires, and there will be no way of knowing. Learn more about Hideout\'s "access-based" approach in\n                            '),
								A2(
								$author$project$Common$Contents$link,
								$author$project$Views$About$sectionToUrl(3),
								'How are chats private on Hideout?')
							]))
					]))
			]));
	return A4($author$project$Views$About$mkSection, 7, 'Why make yet another disposable chat service?', body, model);
};
var $author$project$Views$About$why_privacy = function (model) {
	var body = A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20)
			]),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[$author$project$Views$About$lineSpacing]),
				_List_fromArray(
					[
						$author$project$Common$Contents$plainPara('I always like to use this analogy.'),
						$author$project$Common$Contents$plainPara('Imagine two people, Adam and Bob.'),
						$author$project$Common$Contents$plainPara('If Adam knows a lot about Bob, but Bob knows very little about Adam,'),
						$author$project$Common$Contents$plainPara('Then we say Adam overpowers Bob.'),
						$author$project$Common$Contents$plainPara('\n                        If you can agree with that, then you can see the importance of privacy, on a personal scale.\n                        ')
					])),
				A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[$author$project$Views$About$lineSpacing]),
				_List_fromArray(
					[
						$author$project$Common$Contents$plainPara('But privacy on a personal scale is just a synonym to security.'),
						$author$project$Common$Contents$plainPara('Now replace Adam with governments and corporations,'),
						$author$project$Common$Contents$plainPara('And replace Bob with the mass population. The people.')
					]))
			]));
	return A4($author$project$Views$About$mkSection, 0, 'Why Privacy?', body, model);
};
var $author$project$Views$About$view = F2(
	function (screenWidth, model) {
		var sectionViews = A2(
			$elm$core$List$map,
			function (sectionView) {
				return sectionView(model);
			},
			_List_fromArray(
				[$author$project$Views$About$why_privacy, $author$project$Views$About$use_cases, $author$project$Views$About$threat_model, $author$project$Views$About$how_private, $author$project$Views$About$persist_chat, $author$project$Views$About$troubleshooting, $author$project$Views$About$hideout_vs_apps, $author$project$Views$About$why_another_disp, $author$project$Views$About$self_hosting, $author$project$Views$About$func_prog]));
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					A2($mdgriffith$elm_ui$Element$maximum, 1000, $mdgriffith$elm_ui$Element$fill)),
					$author$project$Views$About$sectionSpacing
				]),
			_Utils_ap(
				_List_fromArray(
					[
						A2($author$project$Common$Contents$link, $author$project$Common$Urls$rootUrl, '<< Hideout Home')
					]),
				sectionViews));
	});
var $author$project$Chat$MessageInput = function (a) {
	return {$: 0, a: a};
};
var $author$project$Chat$MessageSend = {$: 1};
var $author$project$Chat$OnBeginNameChange = {$: 2};
var $author$project$Chat$OnChatInputFocal = function (a) {
	return {$: 7, a: a};
};
var $author$project$Chat$OnEmojiChosen = function (a) {
	return {$: 9, a: a};
};
var $author$project$Chat$OnEmojiScrolled = {$: 10};
var $author$project$Chat$OnEmojiToggled = {$: 8};
var $author$project$Chat$OnFinishNameChange = function (a) {
	return {$: 4, a: a};
};
var $author$project$Chat$OnManualScrolled = {$: 1};
var $author$project$Chat$OnNewMsgHintClicked = {$: 3};
var $author$project$Chat$OnNewNameInput = function (a) {
	return {$: 3, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Above = 0;
var $mdgriffith$elm_ui$Element$above = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 0, element);
};
var $elm$html$Html$audio = _VirtualDom_node('audio');
var $author$project$Common$Styles$buttonStyle = function (padding) {
	return _List_fromArray(
		[
			$mdgriffith$elm_ui$Element$padding(padding),
			$mdgriffith$elm_ui$Element$Border$width(2),
			$mdgriffith$elm_ui$Element$Border$rounded(6)
		]);
};
var $author$project$Utils$Utils$capString = F2(
	function (n, str) {
		return (_Utils_cmp(
			$elm$core$String$length(str),
			n) < 1) ? str : (A2($elm$core$String$left, n, str) + '...');
	});
var $author$project$Views$Chat$capUsername = function (username) {
	return A2($author$project$Utils$Utils$capString, 24, username);
};
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterX = 1;
var $mdgriffith$elm_ui$Element$centerX = $mdgriffith$elm_ui$Internal$Model$AlignX(1);
var $mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterY = 1;
var $mdgriffith$elm_ui$Element$centerY = $mdgriffith$elm_ui$Internal$Model$AlignY(1);
var $author$project$Views$Chat$sideColumnGap = 60;
var $author$project$Views$Chat$sideColumnWidthPx = 400;
var $author$project$Common$Styles$windowPaddingPx = function (windowWidth) {
	return (60 / 2560) * windowWidth;
};
var $author$project$Views$Chat$chatColumnMaxWidthPx = function (windowWidth) {
	return $elm$core$Basics$round(
		((windowWidth - ($author$project$Common$Styles$windowPaddingPx(windowWidth) * 2)) - $author$project$Views$Chat$sideColumnWidthPx) - $author$project$Views$Chat$sideColumnGap);
};
var $mdgriffith$elm_ui$Element$fillPortion = $mdgriffith$elm_ui$Internal$Model$Fill;
var $author$project$Common$Colors$grey = A3($mdgriffith$elm_ui$Element$rgb255, 128, 128, 128);
var $author$project$Emoji$hexToPath = function (hex) {
	return '/static/OpenMoji/48x48/' + (hex + '.png');
};
var $mdgriffith$elm_ui$Element$html = $mdgriffith$elm_ui$Internal$Model$unstyled;
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $mdgriffith$elm_ui$Element$Background$image = function (src) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		A2($elm$virtual_dom$VirtualDom$style, 'background', 'url(\"' + (src + '\") center / cover no-repeat')));
};
var $mdgriffith$elm_ui$Element$Input$Above = 2;
var $mdgriffith$elm_ui$Element$Input$Label = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Element$Input$labelAbove = $mdgriffith$elm_ui$Element$Input$Label(2);
var $author$project$Chat$isMetaMsg = function (msg) {
	var _v0 = msg.bN.bO;
	if (!_v0) {
		return false;
	} else {
		return true;
	}
};
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
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
					L: _List_fromArray(
						[msg]),
					ce: $author$project$Utils$Utils$posixSecToPosix(msg.bY),
					af: msg.af,
					aQ: msg.aQ
				},
				bundles);
			var _v0 = A3(
				$elm$core$Maybe$map2,
				$elm$core$Tuple$pair,
				$elm$core$List$head(bundles),
				$elm$core$List$tail(bundles));
			if (!_v0.$) {
				var _v1 = _v0.a;
				var headBundle = _v1.a;
				var tailBundles = _v1.b;
				if ($author$project$Chat$isMetaMsg(msg) || A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$author$project$Chat$isMetaMsg,
						$elm$core$List$head(headBundle.L)))) {
					return appended;
				} else {
					if (_Utils_eq(msg.af, headBundle.af)) {
						var updatedHeadBundle = _Utils_update(
							headBundle,
							{
								L: A2($elm$core$List$cons, msg, headBundle.L)
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
var $mdgriffith$elm_ui$Element$Font$bold = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.cL);
var $author$project$Chat$isMetaBundle = function (bundle) {
	var _v0 = A2(
		$elm$core$Maybe$map,
		$author$project$Chat$isMetaMsg,
		$elm$core$List$head(bundle.L));
	if ((!_v0.$) && _v0.a) {
		return true;
	} else {
		return false;
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
		case 0:
			var string = problem.a;
			return 'Expecting ' + string;
		case 1:
			return 'Expecting int';
		case 2:
			return 'Expecting hex';
		case 3:
			return 'Expecting octal';
		case 4:
			return 'Expecting binary';
		case 5:
			return 'Expecting float';
		case 6:
			return 'Expecting number';
		case 7:
			return 'Expecting variable';
		case 8:
			var string = problem.a;
			return 'Expecting symbol ' + string;
		case 9:
			var string = problem.a;
			return 'Expecting keyword ' + string;
		case 10:
			return 'Expecting keyword end';
		case 11:
			return 'Unexpected char';
		case 12:
			var problemDescription = problem.a;
			return problemDescription;
		default:
			return 'Bad repeat';
	}
};
var $dillonkearns$elm_markdown$Markdown$Parser$deadEndToString = function (deadEnd) {
	return 'Problem at row ' + ($elm$core$String$fromInt(deadEnd.ek) + ('\n' + $dillonkearns$elm_markdown$Markdown$Parser$problemToString(deadEnd.eb)));
};
var $dillonkearns$elm_markdown$Markdown$Block$Paragraph = function (a) {
	return {$: 5, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Image = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Block$Text = function (a) {
	return {$: 7, a: a};
};
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
var $elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? $elm$core$Maybe$Nothing : $elm$core$List$head(
			A2($elm$core$List$drop, idx, xs));
	});
var $elm$core$String$indices = _String_indexes;
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Utils$Markdown$replaceEmojis = function (str) {
	var colonIndices = A2($elm$core$String$indices, ':', str);
	var firstColonPair = A3(
		$elm$core$Maybe$map2,
		$elm$core$Tuple$pair,
		A2($elm_community$list_extra$List$Extra$getAt, 0, colonIndices),
		A2($elm_community$list_extra$List$Extra$getAt, 1, colonIndices));
	if (firstColonPair.$ === 1) {
		return _List_fromArray(
			[
				$dillonkearns$elm_markdown$Markdown$Block$Text(str)
			]);
	} else {
		var pair = firstColonPair.a;
		var secondColonIndex = pair.b;
		var firstColonIndex = pair.a;
		var possibleEmojiName = A3($elm$core$String$slice, firstColonIndex + 1, secondColonIndex, str);
		var isEmoji = A2($elm$core$List$member, possibleEmojiName, $author$project$Emoji$allHex);
		return (!isEmoji) ? A2(
			$elm$core$List$cons,
			$dillonkearns$elm_markdown$Markdown$Block$Text(
				A2($elm$core$String$left, secondColonIndex, str)),
			$author$project$Utils$Markdown$replaceEmojis(
				A2($elm$core$String$dropLeft, secondColonIndex, str))) : _Utils_ap(
			_List_fromArray(
				[
					$dillonkearns$elm_markdown$Markdown$Block$Text(
					A2($elm$core$String$left, firstColonIndex, str)),
					A3(
					$dillonkearns$elm_markdown$Markdown$Block$Image,
					$author$project$Emoji$hexToPath(possibleEmojiName),
					$elm$core$Maybe$Nothing,
					_List_Nil)
				]),
			$author$project$Utils$Markdown$replaceEmojis(
				A2($elm$core$String$dropLeft, secondColonIndex + 1, str)));
	}
};
var $author$project$Utils$Markdown$handleEmojis_Inlines = function (inlines) {
	return $elm$core$List$concat(
		A2(
			$elm$core$List$map,
			function (inline) {
				if (inline.$ === 7) {
					var str = inline.a;
					return $author$project$Utils$Markdown$replaceEmojis(str);
				} else {
					return _List_fromArray(
						[inline]);
				}
			},
			inlines));
};
var $author$project$Utils$Markdown$handleEmojis_Blocks = $elm$core$List$map(
	function (block) {
		if (block.$ === 5) {
			var inlines = block.a;
			return $dillonkearns$elm_markdown$Markdown$Block$Paragraph(
				$author$project$Utils$Markdown$handleEmojis_Inlines(inlines));
		} else {
			return block;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Block$BlockQuote = function (a) {
	return {$: 3, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Cdata = function (a) {
	return {$: 4, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$CodeBlock = function (a) {
	return {$: 7, a: a};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$CodeBlock = function (a) {
	return {$: 5, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$CodeSpan = function (a) {
	return {$: 6, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$CompletedTask = 2;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Emphasis = function (a) {
	return {$: 3, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Parser$EmptyBlock = {$: 0};
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HardLineBreak = {$: 8};
var $dillonkearns$elm_markdown$Markdown$Block$Heading = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$RawBlock$Html = function (a) {
	return {$: 2, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HtmlBlock = function (a) {
	return {$: 0, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HtmlComment = function (a) {
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$HtmlDeclaration = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Block$HtmlElement = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Block$HtmlInline = function (a) {
	return {$: 0, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$IncompleteTask = 1;
var $dillonkearns$elm_markdown$Markdown$Parser$InlineProblem = function (a) {
	return {$: 2, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Link = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Block$ListItem = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$NoTask = 0;
var $dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph = function (a) {
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$OrderedList = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Problem = function (a) {
	return {$: 12, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$ProcessingInstruction = function (a) {
	return {$: 2, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Strikethrough = function (a) {
	return {$: 5, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Strong = function (a) {
	return {$: 4, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Block$Table = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Block$ThematicBreak = {$: 8};
var $dillonkearns$elm_markdown$Markdown$RawBlock$ThematicBreak = {$: 7};
var $dillonkearns$elm_markdown$Markdown$Block$UnorderedList = function (a) {
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$UnparsedInlines = $elm$core$Basics$identity;
var $dillonkearns$elm_markdown$Markdown$Parser$addReference = F2(
	function (state, linkRef) {
		return {
			T: A2($elm$core$List$cons, linkRef, state.T),
			G: state.G
		};
	});
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
			var _v1 = parseA(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				var _v2 = callback(a);
				var parseB = _v2;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
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
		};
	});
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0;
	return function (s0) {
		var _v1 = parse(s0);
		if (_v1.$ === 1) {
			var x = _v1.b;
			return A2($elm$parser$Parser$Advanced$Bad, false, x);
		} else {
			var a = _v1.b;
			var s1 = _v1.c;
			return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
		}
	};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$BlankLine = {$: 10};
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.ba);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{bq: col, e: s0.e, g: s0.g, b: offset, ek: row, ba: s0.ba});
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
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.ek, s.bq, s);
	};
};
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
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
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $dillonkearns$elm_markdown$Whitespace$isSpaceOrTab = function (_char) {
	switch (_char) {
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
		return {$: 0, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Parser$Token$carriageReturn = A2(
	$elm$parser$Parser$Advanced$Token,
	'\r',
	$elm$parser$Parser$Expecting('a carriage return'));
var $dillonkearns$elm_markdown$Parser$Token$newline = A2(
	$elm$parser$Parser$Advanced$Token,
	'\n',
	$elm$parser$Parser$Expecting('a newline'));
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
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
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {bq: col, c4: contextStack, eb: problem, ek: row};
	});
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.ek, s.bq, x, s.e));
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.b, s.ek, s.bq, s.ba);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{bq: newCol, e: s.e, g: s.g, b: newOffset, ek: newRow, ba: s.ba});
	};
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
						$elm$parser$Parser$Advanced$succeed(0)
					])))
		]));
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
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
		};
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
	return {$: 11, a: a};
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
	switch (_char) {
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
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
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
						A3($elm$core$String$slice, s0.b, s1.b, s0.ba),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.ba),
			s.b) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
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
					$elm$parser$Parser$Advanced$succeed(0)
				]))),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$getChompedString($dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd),
		$dillonkearns$elm_markdown$Helpers$lineEndOrEnd));
var $dillonkearns$elm_markdown$Markdown$RawBlock$Heading = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$RawBlock$IndentedCodeBlock = function (a) {
	return {$: 6, a: a};
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$Table = function (a) {
	return {$: 8, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Table$Table = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Table$TableDelimiterRow = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
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
var $dillonkearns$elm_markdown$Markdown$Table$TableHeader = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
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
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return function (s) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.b, s.ba);
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{bq: 1, e: s.e, g: s.g, b: s.b + 1, ek: s.ek + 1, ba: s.ba}) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{bq: s.bq + 1, e: s.e, g: s.g, b: newOffset, ek: s.ek, ba: s.ba}));
		};
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
					$elm$parser$Parser$Advanced$succeed(0)
				]))),
	$dillonkearns$elm_markdown$Markdown$TableParser$parseCells);
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
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
		var parse = _v0;
		var _v1 = parse(
			{bq: 1, e: _List_Nil, g: 1, b: 0, ek: 1, ba: src});
		if (!_v1.$) {
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
						return {au: alignment, a0: headerCell};
					}),
				headers,
				columnAlignments);
		};
		var combineHeaderAndDelimiter = function (headers) {
			return _Utils_eq(
				$elm$core$List$length(headers),
				$elm$core$List$length(columnAlignments)) ? $elm$core$Result$Ok(
				headersWithAlignment(headers)) : $elm$core$Result$Err(
				'Tables must have the same number of header columns (' + ($elm$core$String$fromInt(
					$elm$core$List$length(headers)) + (') as delimiter columns (' + ($elm$core$String$fromInt(
					$elm$core$List$length(columnAlignments)) + ')'))));
		};
		var _v1 = A2($elm$parser$Parser$Advanced$run, $dillonkearns$elm_markdown$Markdown$TableParser$rowParser, headersRow);
		if (!_v1.$) {
			var headers = _v1.a;
			return combineHeaderAndDelimiter(headers);
		} else {
			return $elm$core$Result$Err('Unable to parse previous line as a table header');
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$completeOrMergeBlocks = F2(
	function (state, newRawBlock) {
		return {
			T: state.T,
			G: function () {
				var _v0 = _Utils_Tuple2(newRawBlock, state.G);
				_v0$9:
				while (true) {
					if (_v0.b.b) {
						switch (_v0.b.a.$) {
							case 5:
								if (_v0.a.$ === 5) {
									var block1 = _v0.a.a;
									var _v1 = _v0.b;
									var block2 = _v1.a.a;
									var rest = _v1.b;
									return A2(
										$elm$core$List$cons,
										$dillonkearns$elm_markdown$Markdown$RawBlock$CodeBlock(
											{
												aa: A2($dillonkearns$elm_markdown$Markdown$Parser$joinStringsPreserveAll, block2.aa, block1.aa),
												dJ: $elm$core$Maybe$Nothing
											}),
										rest);
								} else {
									break _v0$9;
								}
							case 6:
								if (_v0.a.$ === 6) {
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
							case 11:
								switch (_v0.a.$) {
									case 1:
										var body1 = _v0.a.a;
										var _v3 = _v0.b;
										var body2 = _v3.a.a;
										var rest = _v3.b;
										return A2(
											$elm$core$List$cons,
											$dillonkearns$elm_markdown$Markdown$RawBlock$BlockQuote(
												A3($dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith, '\n', body2, body1)),
											rest);
									case 11:
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
							case 1:
								switch (_v0.a.$) {
									case 1:
										var body1 = _v0.a.a;
										var _v5 = _v0.b;
										var body2 = _v5.a.a;
										var rest = _v5.b;
										return A2(
											$elm$core$List$cons,
											$dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
												A3($dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith, '\n', body2, body1)),
											rest);
									case 12:
										if (!_v0.a.a) {
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
									case 9:
										var _v12 = _v0.a.a;
										var text = _v12.a;
										var alignments = _v12.b;
										var _v13 = _v0.b;
										var rawHeaders = _v13.a.a;
										var rest = _v13.b;
										var _v14 = A2(
											$dillonkearns$elm_markdown$Markdown$TableParser$parseHeader,
											A2($dillonkearns$elm_markdown$Markdown$Table$TableDelimiterRow, text, alignments),
											rawHeaders);
										if (!_v14.$) {
											var headers = _v14.a;
											return A2(
												$elm$core$List$cons,
												$dillonkearns$elm_markdown$Markdown$RawBlock$Table(
													A2($dillonkearns$elm_markdown$Markdown$Table$Table, headers, _List_Nil)),
												rest);
										} else {
											return A2(
												$elm$core$List$cons,
												$dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(
													A3($dillonkearns$elm_markdown$Markdown$Parser$joinRawStringsWith, '\n', rawHeaders, text.b1)),
												rest);
										}
									default:
										break _v0$9;
								}
							case 8:
								if (_v0.a.$ === 8) {
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
				return A2($elm$core$List$cons, newRawBlock, state.G);
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
	return {$: 3, a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$Element = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$HtmlParser$Text = function (a) {
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$HtmlParser$expectTagNameCharacter = $elm$parser$Parser$Expecting('at least 1 tag name character');
var $dillonkearns$elm_markdown$HtmlParser$tagNameCharacter = function (c) {
	switch (c) {
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
	return {$: 8, a: a};
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
			_Utils_Tuple2('amp', '&'),
			_Utils_Tuple2('lt', '<'),
			_Utils_Tuple2('gt', '>'),
			_Utils_Tuple2('apos', '\''),
			_Utils_Tuple2('quot', '\"')
		]));
var $elm$core$Char$fromCode = _Char_fromCode;
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (!maybe.$) {
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
				switch (_char) {
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
	return function (s) {
		return A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $dillonkearns$elm_markdown$HtmlParser$escapedChar = function (end_) {
	var process = function (entityStr) {
		var _v0 = $dillonkearns$elm_markdown$HtmlParser$decodeEscape(entityStr);
		if (!_v0.$) {
			var c = _v0.a;
			return $elm$parser$Parser$Advanced$succeed(c);
		} else {
			var e = _v0.a;
			return $elm$parser$Parser$Advanced$problem(e);
		}
	};
	var isEntityChar = function (c) {
		return (!_Utils_eq(c, end_)) && (c !== ';');
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
		return (!_Utils_eq(c, closingChar)) && (c !== '&');
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
				$dillonkearns$elm_markdown$HtmlParser$textString('\"'),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\"'))),
			A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\'')),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$dillonkearns$elm_markdown$HtmlParser$textString('\''),
				$dillonkearns$elm_markdown$HtmlParser$symbol('\'')))
		]));
var $dillonkearns$elm_markdown$HtmlParser$keepOldest = F2(
	function (_new, mValue) {
		if (!mValue.$) {
			var v = mValue.a;
			return $elm$core$Maybe$Just(v);
		} else {
			return $elm$core$Maybe$Just(_new);
		}
	});
var $dillonkearns$elm_markdown$HtmlParser$isWhitespace = function (c) {
	switch (c) {
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
					{bP: key, bg: value},
					accum);
			}),
		_List_Nil),
	A2($elm$parser$Parser$Advanced$loop, $elm$core$Dict$empty, $dillonkearns$elm_markdown$HtmlParser$attributesStep));
var $elm$parser$Parser$Advanced$chompUntilEndOr = function (str) {
	return function (s) {
		var _v0 = A5(_Parser_findSubString, str, s.b, s.ek, s.bq, s.ba);
		var newOffset = _v0.a;
		var newRow = _v0.b;
		var newCol = _v0.c;
		var adjustedOffset = (newOffset < 0) ? $elm$core$String$length(s.ba) : newOffset;
		return A3(
			$elm$parser$Parser$Advanced$Good,
			_Utils_cmp(s.b, adjustedOffset) < 0,
			0,
			{bq: newCol, e: s.e, g: s.g, b: adjustedOffset, ek: newRow, ba: s.ba});
	};
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
			return _Utils_eq(startTagName, endTagName) ? $elm$parser$Parser$Advanced$succeed(0) : $dillonkearns$elm_markdown$HtmlParser$fail('tag name mismatch: ' + (startTagName + (' and ' + endTagName)));
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
	return {$: 2, a: a};
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
		return {$: 5, a: a, b: b};
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
	return {$: 4, a: a};
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
	switch (c) {
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
			return $elm$parser$Parser$Advanced$Loop(0);
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
			return $elm$parser$Parser$Advanced$Loop(0);
		},
		$dillonkearns$elm_markdown$HtmlParser$escapedChar('<')),
		$elm$parser$Parser$Advanced$succeed(
		$elm$parser$Parser$Advanced$Done(0))
	]);
var $dillonkearns$elm_markdown$HtmlParser$textNodeStringStep = function (_v0) {
	return $elm$parser$Parser$Advanced$oneOf($dillonkearns$elm_markdown$HtmlParser$textNodeStringStepOptions);
};
var $dillonkearns$elm_markdown$HtmlParser$textNodeString = $elm$parser$Parser$Advanced$getChompedString(
	A2($elm$parser$Parser$Advanced$loop, 0, $dillonkearns$elm_markdown$HtmlParser$textNodeStringStep));
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
var $dillonkearns$elm_markdown$HtmlParser$html = $dillonkearns$elm_markdown$HtmlParser$cyclic$html();
$dillonkearns$elm_markdown$HtmlParser$cyclic$html = function () {
	return $dillonkearns$elm_markdown$HtmlParser$html;
};
var $dillonkearns$elm_markdown$HtmlParser$element = $dillonkearns$elm_markdown$HtmlParser$cyclic$element();
$dillonkearns$elm_markdown$HtmlParser$cyclic$element = function () {
	return $dillonkearns$elm_markdown$HtmlParser$element;
};
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
			return $dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(rawLine);
		}),
	$dillonkearns$elm_markdown$Helpers$chompUntilLineEndOrEnd);
var $dillonkearns$elm_markdown$Markdown$Parser$openBlockOrParagraphParser = A2($elm$parser$Parser$Advanced$ignorer, $dillonkearns$elm_markdown$Markdown$Parser$innerParagraphParser, $dillonkearns$elm_markdown$Helpers$lineEndOrEnd);
var $dillonkearns$elm_markdown$Markdown$RawBlock$OrderedListBlock = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
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
				A2($elm$core$List$map, $elm$core$Basics$identity, unparsedLines));
		},
		$dillonkearns$elm_markdown$Markdown$OrderedList$parser(previousWasBody));
};
var $dillonkearns$elm_markdown$Markdown$Inline$CodeInline = function (a) {
	return {$: 2, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Inline$Emphasis = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$Inline$HardLineBreak = {$: 1};
var $dillonkearns$elm_markdown$Markdown$Inline$HtmlInline = function (a) {
	return {$: 5, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Inline$Image = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Inline$Link = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $dillonkearns$elm_markdown$Markdown$Inline$Strikethrough = function (a) {
	return {$: 7, a: a};
};
var $dillonkearns$elm_markdown$Markdown$Inline$Text = function (a) {
	return {$: 0, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$matchToInline = function (_v0) {
	var match = _v0;
	var _v1 = match.j;
	switch (_v1.$) {
		case 0:
			return $dillonkearns$elm_markdown$Markdown$Inline$Text(match.eR);
		case 1:
			return $dillonkearns$elm_markdown$Markdown$Inline$HardLineBreak;
		case 2:
			return $dillonkearns$elm_markdown$Markdown$Inline$CodeInline(match.eR);
		case 3:
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
		case 4:
			var _v3 = _v1.a;
			var url = _v3.a;
			var maybeTitle = _v3.b;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Inline$Link,
				url,
				maybeTitle,
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.n));
		case 5:
			var _v4 = _v1.a;
			var url = _v4.a;
			var maybeTitle = _v4.b;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Inline$Image,
				url,
				maybeTitle,
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.n));
		case 6:
			var model = _v1.a;
			return $dillonkearns$elm_markdown$Markdown$Inline$HtmlInline(model);
		case 7:
			var length = _v1.a;
			return A2(
				$dillonkearns$elm_markdown$Markdown$Inline$Emphasis,
				length,
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.n));
		default:
			return $dillonkearns$elm_markdown$Markdown$Inline$Strikethrough(
				$dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines(match.n));
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$matchesToInlines = function (matches) {
	return A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$InlineParser$matchToInline, matches);
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$Match = $elm$core$Basics$identity;
var $dillonkearns$elm_markdown$Markdown$InlineParser$prepareChildMatch = F2(
	function (parentMatch, childMatch) {
		return {f: childMatch.f - parentMatch.p, n: childMatch.n, h: childMatch.h - parentMatch.p, eR: childMatch.eR, t: childMatch.t - parentMatch.p, p: childMatch.p - parentMatch.p, j: childMatch.j};
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$addChild = F2(
	function (parentMatch, childMatch) {
		return {
			f: parentMatch.f,
			n: A2(
				$elm$core$List$cons,
				A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareChildMatch, parentMatch, childMatch),
				parentMatch.n),
			h: parentMatch.h,
			eR: parentMatch.eR,
			t: parentMatch.t,
			p: parentMatch.p,
			j: parentMatch.j
		};
	});
var $elm$core$List$sortBy = _List_sortBy;
var $dillonkearns$elm_markdown$Markdown$InlineParser$organizeChildren = function (_v4) {
	var match = _v4;
	return {
		f: match.f,
		n: $dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatches(match.n),
		h: match.h,
		eR: match.eR,
		t: match.t,
		p: match.p,
		j: match.j
	};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$organizeMatches = function (matches) {
	var _v2 = A2(
		$elm$core$List$sortBy,
		function (_v3) {
			var match = _v3;
			return match.h;
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
			var prevMatch = _v0;
			if (!remaining.b) {
				return A2(
					$elm$core$List$cons,
					$dillonkearns$elm_markdown$Markdown$InlineParser$organizeChildren(prevMatch),
					matchesTail);
			} else {
				var match = remaining.a;
				var rest = remaining.b;
				if (_Utils_cmp(prevMatch.f, match.h) < 1) {
					var $temp$remaining = rest,
						$temp$_v0 = match,
						$temp$matchesTail = A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$InlineParser$organizeChildren(prevMatch),
						matchesTail);
					remaining = $temp$remaining;
					_v0 = $temp$_v0;
					matchesTail = $temp$matchesTail;
					continue organizeMatchesHelp;
				} else {
					if ((_Utils_cmp(prevMatch.h, match.h) < 0) && (_Utils_cmp(prevMatch.f, match.f) > 0)) {
						var $temp$remaining = rest,
							$temp$_v0 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$addChild, prevMatch, match),
							$temp$matchesTail = matchesTail;
						remaining = $temp$remaining;
						_v0 = $temp$_v0;
						matchesTail = $temp$matchesTail;
						continue organizeMatchesHelp;
					} else {
						var $temp$remaining = rest,
							$temp$_v0 = prevMatch,
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
var $dillonkearns$elm_markdown$Markdown$InlineParser$NormalType = {$: 0};
var $dillonkearns$elm_markdown$Markdown$Helpers$containsAmpersand = function (string) {
	return A2($elm$core$String$contains, '&', string);
};
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {a: index, al: match, dY: number, bc: submatches};
	});
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{cU: false, dU: false},
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
	var _v0 = match.bc;
	if (_v0.b && (!_v0.a.$)) {
		var first = _v0.a.a;
		var _v1 = $elm$core$String$toInt(first);
		if (!_v1.$) {
			var v = _v1.a;
			return $dillonkearns$elm_markdown$Markdown$Entity$validUnicode(v);
		} else {
			return match.al;
		}
	} else {
		return match.al;
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
	var _v0 = match.bc;
	if (_v0.b && (!_v0.a.$)) {
		var first = _v0.a.a;
		var _v1 = A2($elm$core$Dict$get, first, $dillonkearns$elm_markdown$Markdown$Entity$entities);
		if (!_v1.$) {
			var code = _v1.a;
			return $elm$core$String$fromChar(
				$elm$core$Char$fromCode(code));
		} else {
			return match.al;
		}
	} else {
		return match.al;
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
		var _v0 = regexMatch.bc;
		if (((_v0.b && (!_v0.a.$)) && _v0.b.b) && (!_v0.b.a.$)) {
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
			return regexMatch.al;
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
	var _v0 = match.bc;
	if (_v0.b && (!_v0.a.$)) {
		var first = _v0.a.a;
		return $dillonkearns$elm_markdown$Markdown$Entity$validUnicode(
			$dillonkearns$elm_markdown$Markdown$Entity$hexToInt(first));
	} else {
		return match.al;
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
	return {
		f: 0,
		n: _List_Nil,
		h: 0,
		eR: $dillonkearns$elm_markdown$Markdown$Helpers$formatStr(text),
		t: 0,
		p: 0,
		j: $dillonkearns$elm_markdown$Markdown$InlineParser$NormalType
	};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatch = F3(
	function (rawText, _v2, parsedMatches) {
		var matchModel = _v2;
		var updtMatch = {
			f: matchModel.f,
			n: A3($dillonkearns$elm_markdown$Markdown$InlineParser$parseTextMatches, matchModel.eR, _List_Nil, matchModel.n),
			h: matchModel.h,
			eR: matchModel.eR,
			t: matchModel.t,
			p: matchModel.p,
			j: matchModel.j
		};
		if (!parsedMatches.b) {
			var finalStr = A2($elm$core$String$dropLeft, matchModel.f, rawText);
			return $elm$core$String$isEmpty(finalStr) ? _List_fromArray(
				[updtMatch]) : _List_fromArray(
				[
					updtMatch,
					$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(finalStr)
				]);
		} else {
			var matchHead = parsedMatches.a;
			var matchesTail = parsedMatches.b;
			var _v4 = matchHead.j;
			if (!_v4.$) {
				return A2($elm$core$List$cons, updtMatch, parsedMatches);
			} else {
				return _Utils_eq(matchModel.f, matchHead.h) ? A2($elm$core$List$cons, updtMatch, parsedMatches) : ((_Utils_cmp(matchModel.f, matchHead.h) < 0) ? A2(
					$elm$core$List$cons,
					updtMatch,
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(
							A3($elm$core$String$slice, matchModel.f, matchHead.h, rawText)),
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
					var matchModel = parsedMatches.a;
					return (matchModel.h > 0) ? A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$InlineParser$normalMatch(
							A2($elm$core$String$left, matchModel.h, rawText)),
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
var $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketOpen = {$: 4};
var $dillonkearns$elm_markdown$Markdown$Helpers$isEven = function (_int) {
	return !A2($elm$core$Basics$modBy, 2, _int);
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken = function (regMatch) {
	var _v0 = regMatch.bc;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var delimiter = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
			{a: regMatch.a + backslashesLength, a1: 1, c: $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketOpen}) : $elm$core$Maybe$Nothing;
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
	return {$: 5, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$Escaped = 0;
var $dillonkearns$elm_markdown$Markdown$InlineParser$NotEscaped = 1;
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken = function (regMatch) {
	var _v0 = regMatch.bc;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $elm$core$Maybe$Just(
			{
				a: regMatch.a + backslashesLength,
				a1: 1,
				c: $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketClose(1) : $dillonkearns$elm_markdown$Markdown$InlineParser$AngleBracketClose(0)
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
		return {$: 7, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isPunctuation = function (c) {
	switch (c) {
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
	switch (c) {
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
	if (!mstring.$) {
		var string = mstring.a;
		return ($elm$core$String$isEmpty(string) || $dillonkearns$elm_markdown$Markdown$InlineParser$containSpace(string)) ? 0 : ($dillonkearns$elm_markdown$Markdown$InlineParser$containPunctuation(string) ? 1 : 2);
	} else {
		return 0;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken = F3(
	function (_char, rawText, regMatch) {
		var _v0 = regMatch.bc;
		if ((((_v0.b && _v0.b.b) && _v0.b.b.b) && (!_v0.b.b.a.$)) && _v0.b.b.b.b) {
			var maybeBackslashes = _v0.a;
			var _v1 = _v0.b;
			var maybeLeftFringe = _v1.a;
			var _v2 = _v1.b;
			var delimiter = _v2.a.a;
			var _v3 = _v2.b;
			var maybeRightFringe = _v3.a;
			var rFringeRank = $dillonkearns$elm_markdown$Markdown$InlineParser$getFringeRank(maybeRightFringe);
			var leftFringeLength = function () {
				if (!maybeLeftFringe.$) {
					var left = maybeLeftFringe.a;
					return $elm$core$String$length(left);
				} else {
					return 0;
				}
			}();
			var mLeftFringe = ((!(!regMatch.a)) && (!leftFringeLength)) ? $elm$core$Maybe$Just(
				A3($elm$core$String$slice, regMatch.a - 1, regMatch.a, rawText)) : maybeLeftFringe;
			var backslashesLength = function () {
				if (!maybeBackslashes.$) {
					var backslashes = maybeBackslashes.a;
					return $elm$core$String$length(backslashes);
				} else {
					return 0;
				}
			}();
			var isEscaped = ((!$dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) && (!leftFringeLength)) || function () {
				if ((!mLeftFringe.$) && (mLeftFringe.a === '\\')) {
					return true;
				} else {
					return false;
				}
			}();
			var delimiterLength = isEscaped ? ($elm$core$String$length(delimiter) - 1) : $elm$core$String$length(delimiter);
			var lFringeRank = isEscaped ? 1 : $dillonkearns$elm_markdown$Markdown$InlineParser$getFringeRank(mLeftFringe);
			if ((delimiterLength <= 0) || ((_char === '_') && ((lFringeRank === 2) && (rFringeRank === 2)))) {
				return $elm$core$Maybe$Nothing;
			} else {
				var index = ((regMatch.a + backslashesLength) + leftFringeLength) + (isEscaped ? 1 : 0);
				return $elm$core$Maybe$Just(
					{
						a: index,
						a1: delimiterLength,
						c: A2(
							$dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisToken,
							_char,
							{aJ: lFringeRank, aL: rFringeRank})
					});
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		A2($dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken, '*', str),
		A2($elm$regex$Regex$find, $dillonkearns$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex, str));
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$codeTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\`+)'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$CodeToken = function (a) {
	return {$: 0, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToCodeToken = function (regMatch) {
	var _v0 = regMatch.bc;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backtick = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $elm$core$Maybe$Just(
			{
				a: regMatch.a + backslashesLength,
				a1: $elm$core$String$length(backtick),
				c: $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $dillonkearns$elm_markdown$Markdown$InlineParser$CodeToken(1) : $dillonkearns$elm_markdown$Markdown$InlineParser$CodeToken(0)
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
var $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken = {$: 9};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken = function (regMatch) {
	var _v0 = regMatch.bc;
	_v0$2:
	while (true) {
		if (_v0.b) {
			if (!_v0.a.$) {
				var backslashes = _v0.a.a;
				var backslashesLength = $elm$core$String$length(backslashes);
				return (!$dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) ? $elm$core$Maybe$Just(
					{a: (regMatch.a + backslashesLength) - 1, a1: 2, c: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : $elm$core$Maybe$Nothing;
			} else {
				if (_v0.b.b && (!_v0.b.a.$)) {
					var _v1 = _v0.b;
					return $elm$core$Maybe$Just(
						{
							a: regMatch.a,
							a1: $elm$core$String$length(regMatch.al),
							c: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken
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
	var _v0 = regMatch.bc;
	_v0$2:
	while (true) {
		if (_v0.b) {
			if (!_v0.a.$) {
				var backslashes = _v0.a.a;
				var backslashesLength = $elm$core$String$length(backslashes);
				return $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
					{a: regMatch.a + backslashesLength, a1: 1, c: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : $elm$core$Maybe$Just(
					{a: (regMatch.a + backslashesLength) - 1, a1: 2, c: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken});
			} else {
				if (_v0.b.b) {
					var _v1 = _v0.b;
					var maybeSpaces = _v1.a;
					return $elm$core$Maybe$Just(
						{
							a: regMatch.a,
							a1: $elm$core$String$length(regMatch.al),
							c: $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakToken
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
var $dillonkearns$elm_markdown$Markdown$InlineParser$SquareBracketClose = {$: 3};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken = function (regMatch) {
	var _v0 = regMatch.bc;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
			{a: regMatch.a + backslashesLength, a1: 1, c: $dillonkearns$elm_markdown$Markdown$InlineParser$SquareBracketClose}) : $elm$core$Maybe$Nothing;
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
var $dillonkearns$elm_markdown$Markdown$InlineParser$Active = 0;
var $dillonkearns$elm_markdown$Markdown$InlineParser$ImageOpenToken = {$: 2};
var $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken = function (a) {
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken = function (regMatch) {
	var _v0 = regMatch.bc;
	if (((_v0.b && _v0.b.b) && _v0.b.b.b) && (!_v0.b.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var maybeImageOpen = _v1.a;
		var _v2 = _v1.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		var isEscaped = !$dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength);
		var index = isEscaped ? ((regMatch.a + backslashesLength) + 1) : (regMatch.a + backslashesLength);
		if (isEscaped) {
			if (!maybeImageOpen.$) {
				return $elm$core$Maybe$Just(
					{
						a: index,
						a1: 1,
						c: $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken(0)
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		} else {
			if (!maybeImageOpen.$) {
				return $elm$core$Maybe$Just(
					{a: index, a1: 2, c: $dillonkearns$elm_markdown$Markdown$InlineParser$ImageOpenToken});
			} else {
				return $elm$core$Maybe$Just(
					{
						a: index,
						a1: 1,
						c: $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken(0)
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
	return {$: 10, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToStrikethroughToken = function (regMatch) {
	var _v0 = regMatch.bc;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var tilde = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		var _v2 = $dillonkearns$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? _Utils_Tuple2(
			$elm$core$String$length(tilde),
			$dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughToken(1)) : _Utils_Tuple2(
			$elm$core$String$length(tilde),
			$dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughToken(0));
		var length = _v2.a;
		var meaning = _v2.b;
		return $elm$core$Maybe$Just(
			{a: regMatch.a + backslashesLength, a1: length, c: meaning});
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
		A2($dillonkearns$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken, '_', str),
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
				return (_Utils_cmp(lfirst.a, rfirst.a) < 0) ? A2(
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
var $dillonkearns$elm_markdown$Markdown$InlineParser$CodeType = {$: 2};
var $dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisType = function (a) {
	return {$: 7, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$HtmlType = function (a) {
	return {$: 6, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType = function (a) {
	return {$: 5, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$Inactive = 1;
var $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType = function (a) {
	return {$: 4, a: a};
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$StrikethroughType = {$: 8};
var $dillonkearns$elm_markdown$Markdown$InlineParser$AutolinkType = function (a) {
	return {$: 3, a: a};
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
				match.al,
				$elm$url$Url$percentDecode(match.al));
		}));
var $dillonkearns$elm_markdown$Markdown$InlineParser$urlRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^([A-Za-z][A-Za-z0-9.+\\-]{1,31}:[^<>\\x00-\\x20]*)$'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$autolinkToMatch = function (_v0) {
	var match = _v0;
	return A2($elm$regex$Regex$contains, $dillonkearns$elm_markdown$Markdown$InlineParser$urlRegex, match.eR) ? $elm$core$Result$Ok(
		_Utils_update(
			match,
			{
				j: $dillonkearns$elm_markdown$Markdown$InlineParser$AutolinkType(
					_Utils_Tuple2(
						match.eR,
						$dillonkearns$elm_markdown$Markdown$InlineParser$encodeUrl(match.eR)))
			})) : $elm$core$Result$Err(match);
};
var $elm$regex$Regex$findAtMost = _Regex_findAtMost;
var $dillonkearns$elm_markdown$Markdown$Helpers$insideSquareBracketRegex = '[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*';
var $dillonkearns$elm_markdown$Markdown$InlineParser$refLabelRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\[\\s*(' + ($dillonkearns$elm_markdown$Markdown$Helpers$insideSquareBracketRegex + ')\\s*\\]')));
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
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
			return $elm$core$String$isEmpty(str) ? matchModel.eR : str;
		}(
			A2(
				$elm$core$Maybe$withDefault,
				matchModel.eR,
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Maybe$Nothing,
					A2(
						$elm$core$Maybe$andThen,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.bc;
							},
							$elm$core$List$head),
						maybeRegexMatch))));
		var _v0 = A2(
			$elm$core$Dict$get,
			$dillonkearns$elm_markdown$Markdown$Helpers$prepareRefLabel(refLabel),
			references);
		if (_v0.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v1 = _v0.a;
			var rawUrl = _v1.a;
			var maybeTitle = _v1.b;
			var type_ = function () {
				var _v3 = matchModel.j;
				if (_v3.$ === 5) {
					return $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType(
						A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle, rawUrl, maybeTitle));
				} else {
					return $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType(
						A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle, rawUrl, maybeTitle));
				}
			}();
			var regexMatchLength = function () {
				if (!maybeRegexMatch.$) {
					var match = maybeRegexMatch.a.al;
					return $elm$core$String$length(match);
				} else {
					return 0;
				}
			}();
			return $elm$core$Maybe$Just(
				_Utils_update(
					matchModel,
					{f: matchModel.f + regexMatchLength, j: type_}));
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineReferences = F3(
	function (remainText, _v0, references) {
		var tempMatch = _v0;
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
			if (!maybeFound.$) {
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
		var _v0 = regexMatch.bc;
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
				return _Utils_update(
					matchModel,
					{
						f: matchModel.f + $elm$core$String$length(regexMatch.al),
						j: function () {
							var _v5 = matchModel.j;
							if (_v5.$ === 5) {
								return $dillonkearns$elm_markdown$Markdown$InlineParser$ImageType;
							} else {
								return $dillonkearns$elm_markdown$Markdown$InlineParser$LinkType;
							}
						}()(
							A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle, rawUrl, maybeTitle))
					});
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
		var tempMatch = _v0;
		var _v1 = A3($elm$regex$Regex$findAtMost, 1, $dillonkearns$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex, remainText);
		if (_v1.b) {
			var first = _v1.a;
			var _v2 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch, tempMatch, first);
			if (!_v2.$) {
				var match = _v2.a;
				return $elm$core$Maybe$Just(match);
			} else {
				return A3($dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineReferences, remainText, tempMatch, refs);
			}
		} else {
			return A3($dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineReferences, remainText, tempMatch, refs);
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping = F2(
	function (_v0, remainMatches) {
		var match = _v0;
		var overlappingMatches = $elm$core$List$filter(
			function (_v1) {
				var testMatch = _v1;
				return (_Utils_cmp(match.f, testMatch.h) > 0) && (_Utils_cmp(match.f, testMatch.f) < 0);
			});
		return ($elm$core$List$isEmpty(remainMatches) || $elm$core$List$isEmpty(
			overlappingMatches(remainMatches))) ? $elm$core$Maybe$Just(
			A2($elm$core$List$cons, match, remainMatches)) : $elm$core$Maybe$Nothing;
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$emailRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^([a-zA-Z0-9.!#$%&\'*+\\/=?^_`{|}~\\-]+@[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*)$'));
var $dillonkearns$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch = function (_v0) {
	var match = _v0;
	return A2($elm$regex$Regex$contains, $dillonkearns$elm_markdown$Markdown$InlineParser$emailRegex, match.eR) ? $elm$core$Result$Ok(
		_Utils_update(
			match,
			{
				j: $dillonkearns$elm_markdown$Markdown$InlineParser$AutolinkType(
					_Utils_Tuple2(
						match.eR,
						'mailto:' + $dillonkearns$elm_markdown$Markdown$InlineParser$encodeUrl(match.eR)))
			})) : $elm$core$Result$Err(match);
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
		return {$: 6, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$NotOpening = 1;
var $elm$parser$Parser$Advanced$getOffset = function (s) {
	return A3($elm$parser$Parser$Advanced$Good, false, s.b, s);
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$htmlToToken = F2(
	function (rawText, _v0) {
		var match = _v0;
		var consumedCharacters = A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$keeper,
				A2(
					$elm$parser$Parser$Advanced$keeper,
					$elm$parser$Parser$Advanced$succeed(
						F3(
							function (startOffset, htmlTag, endOffset) {
								return {bE: htmlTag, a1: endOffset - startOffset};
							})),
					$elm$parser$Parser$Advanced$getOffset),
				$dillonkearns$elm_markdown$HtmlParser$html),
			$elm$parser$Parser$Advanced$getOffset);
		var parsed = A2(
			$elm$parser$Parser$Advanced$run,
			consumedCharacters,
			A2($elm$core$String$dropLeft, match.h, rawText));
		if (!parsed.$) {
			var htmlTag = parsed.a.bE;
			var length = parsed.a.a1;
			var htmlToken = A2($dillonkearns$elm_markdown$Markdown$InlineParser$HtmlToken, 1, htmlTag);
			return $elm$core$Maybe$Just(
				{a: match.h, a1: length, c: htmlToken});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Helpers$ifError = F2(
	function (_function, result) {
		if (!result.$) {
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
		var _v0 = openToken.c;
		if (!_v0.$) {
			if (!_v0.a) {
				var _v1 = _v0.a;
				return _Utils_eq(openToken.a1 - 1, closeToken.a1);
			} else {
				var _v2 = _v0.a;
				return _Utils_eq(openToken.a1, closeToken.a1);
			}
		} else {
			return false;
		}
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken = function (token) {
	var _v0 = token.c;
	switch (_v0.$) {
		case 1:
			return true;
		case 2:
			return true;
		default:
			return false;
	}
};
var $dillonkearns$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken = F2(
	function (closeToken, openToken) {
		var _v0 = openToken.c;
		if (_v0.$ === 7) {
			var openChar = _v0.a;
			var open = _v0.b;
			var _v1 = closeToken.c;
			if (_v1.$ === 7) {
				var closeChar = _v1.a;
				var close = _v1.b;
				return _Utils_eq(openChar, closeChar) ? ((_Utils_eq(open.aJ, open.aL) || _Utils_eq(close.aJ, close.aL)) ? (!(!A2($elm$core$Basics$modBy, 3, closeToken.a1 + openToken.a1))) : true) : false;
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
			var _v1 = openToken.c;
			if (_v1.$ === 10) {
				if (!_v1.a) {
					var _v2 = _v1.a;
					return _Utils_Tuple2(true, openToken.a1 - 1);
				} else {
					var _v3 = _v1.a;
					return _Utils_Tuple2(true, openToken.a1);
				}
			} else {
				return _Utils_Tuple2(false, 0);
			}
		}();
		var openTokenIsStrikethrough = _v0.a;
		var openTokenLength = _v0.b;
		var _v4 = function () {
			var _v5 = closeToken.c;
			if (_v5.$ === 10) {
				if (!_v5.a) {
					var _v6 = _v5.a;
					return _Utils_Tuple2(true, closeToken.a1 - 1);
				} else {
					var _v7 = _v5.a;
					return _Utils_Tuple2(true, closeToken.a1);
				}
			} else {
				return _Utils_Tuple2(false, 0);
			}
		}();
		var closeTokenIsStrikethrough = _v4.a;
		var closeTokenLength = _v4.b;
		return closeTokenIsStrikethrough && (openTokenIsStrikethrough && _Utils_eq(closeTokenLength, openTokenLength));
	});
var $dillonkearns$elm_markdown$Markdown$InlineParser$HardLineBreakType = {$: 1};
var $dillonkearns$elm_markdown$Markdown$InlineParser$tokenToMatch = F2(
	function (token, type_) {
		return {f: token.a + token.a1, n: _List_Nil, h: token.a, eR: '', t: 0, p: 0, j: type_};
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
				var _v1 = token.c;
				if (_v1.$ === 9) {
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
		var match = _v0;
		return A2(
			$elm$core$List$filter,
			function (token) {
				return _Utils_cmp(token.a, match.f) > -1;
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
		if (result.$ === 1) {
			var tempMatch = result.a;
			if (escaped === 1) {
				var _v49 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$htmlToToken, rawText, tempMatch);
				if (!_v49.$) {
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
				var _v38 = token.c;
				switch (_v38.$) {
					case 0:
						var isEscaped = _v38.a;
						var _v39 = A2(
							$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
							$dillonkearns$elm_markdown$Markdown$InlineParser$isCodeTokenPair(token),
							tokens);
						if (!_v39.$) {
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
					case 5:
						var isEscaped = _v38.a;
						var isAngleBracketOpen = function (_v45) {
							var meaning = _v45.c;
							if (meaning.$ === 4) {
								return true;
							} else {
								return false;
							}
						};
						var _v41 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$findToken, isAngleBracketOpen, tokens);
						if (!_v41.$) {
							var found = _v41.a;
							var _v42 = A6($dillonkearns$elm_markdown$Markdown$InlineParser$angleBracketsToMatch, token, isEscaped, matches, references, rawText, found);
							if (!_v42.$) {
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
			var _v35 = openToken.c;
			if ((!_v35.$) && (!_v35.a)) {
				var _v36 = _v35.a;
				return _Utils_update(
					openToken,
					{a: openToken.a + 1, a1: openToken.a1 - 1});
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
				var _v29 = token.c;
				if (_v29.$ === 7) {
					var _char = _v29.a;
					var leftFringeRank = _v29.b.aJ;
					var rightFringeRank = _v29.b.aL;
					if (_Utils_eq(leftFringeRank, rightFringeRank)) {
						if ((!(!rightFringeRank)) && ((_char !== '_') || (rightFringeRank === 1))) {
							var _v30 = A2(
								$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
								$dillonkearns$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
								tokens);
							if (!_v30.$) {
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
							if (!_v32.$) {
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
		var remainLength = openToken.a1 - closeToken.a1;
		var updt = (!remainLength) ? {az: closeToken, an: openToken, aK: remainTokens, aP: tokensTail} : ((remainLength > 0) ? {
			az: closeToken,
			an: _Utils_update(
				openToken,
				{a: openToken.a + remainLength, a1: closeToken.a1}),
			aK: A2(
				$elm$core$List$cons,
				_Utils_update(
					openToken,
					{a1: remainLength}),
				remainTokens),
			aP: tokensTail
		} : {
			az: _Utils_update(
				closeToken,
				{a1: openToken.a1}),
			an: openToken,
			aK: remainTokens,
			aP: A2(
				$elm$core$List$cons,
				_Utils_update(
					closeToken,
					{a: closeToken.a + openToken.a1, a1: -remainLength}),
				tokensTail)
		});
		var match = A7(
			$dillonkearns$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
			references,
			rawText,
			function (s) {
				return s;
			},
			$dillonkearns$elm_markdown$Markdown$InlineParser$EmphasisType(updt.an.a1),
			updt.an,
			updt.az,
			$elm$core$List$reverse(innerTokens));
		return _Utils_Tuple3(updt.aP, match, updt.aK);
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
				var _v23 = token.c;
				if (_v23.$ === 6) {
					var isOpen = _v23.a;
					var htmlModel = _v23.b;
					if (isOpen === 1) {
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
						if (_v25.$ === 1) {
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
				var _v18 = token.c;
				if (_v18.$ === 3) {
					var _v19 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$findToken, $dillonkearns$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken, tokens);
					if (!_v19.$) {
						var found = _v19.a;
						var _v20 = A6($dillonkearns$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch, token, tokensTail, matches, references, rawText, found);
						if (!_v20.$) {
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
		var remainText = A2($elm$core$String$dropLeft, closeToken.a + 1, rawText);
		var inactivateLinkOpenToken = function (token) {
			var _v16 = token.c;
			if (_v16.$ === 1) {
				return _Utils_update(
					token,
					{
						c: $dillonkearns$elm_markdown$Markdown$InlineParser$LinkOpenToken(1)
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
		var _v9 = openToken.c;
		switch (_v9.$) {
			case 2:
				var tempMatch = findTempMatch(false);
				var _v10 = A3($dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType, remainText, tempMatch, references);
				if (_v10.$ === 1) {
					return $elm$core$Maybe$Just(removeOpenToken);
				} else {
					var match = _v10.a;
					var _v11 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping, match, oldMatches);
					if (!_v11.$) {
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
			case 1:
				if (!_v9.a) {
					var _v12 = _v9.a;
					var tempMatch = findTempMatch(true);
					var _v13 = A3($dillonkearns$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType, remainText, tempMatch, references);
					if (_v13.$ === 1) {
						return $elm$core$Maybe$Just(removeOpenToken);
					} else {
						var match = _v13.a;
						var _v14 = A2($dillonkearns$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping, match, oldMatches);
						if (!_v14.$) {
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
				var _v5 = token.c;
				if (_v5.$ === 10) {
					var isEscaped = _v5.a;
					var _v6 = A2(
						$dillonkearns$elm_markdown$Markdown$InlineParser$findToken,
						$dillonkearns$elm_markdown$Markdown$InlineParser$isStrikethroughTokenPair(token),
						tokens);
					if (!_v6.$) {
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
			var _v2 = openToken.c;
			if ((_v2.$ === 10) && (!_v2.a)) {
				var _v3 = _v2.a;
				return _Utils_update(
					openToken,
					{a: openToken.a + 1, a1: openToken.a1 - 1});
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
		var textStart = openToken.a + openToken.a1;
		var textEnd = closeToken.a;
		var text = processText(
			A3($elm$core$String$slice, textStart, textEnd, rawText));
		var start = openToken.a;
		var end = closeToken.a + closeToken.a1;
		var match = {f: end, n: _List_Nil, h: start, eR: text, t: textEnd, p: textStart, j: type_};
		var matches = A2(
			$elm$core$List$map,
			function (_v0) {
				var matchModel = _v0;
				return A2($dillonkearns$elm_markdown$Markdown$InlineParser$prepareChildMatch, match, matchModel);
			},
			A4($dillonkearns$elm_markdown$Markdown$InlineParser$tokensToMatches, innerTokens, _List_Nil, references, rawText));
		return {f: end, n: matches, h: start, eR: text, t: textEnd, p: textStart, j: type_};
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
						return $elm$core$Char$isAlphaNum(c) || (c === '-');
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
				return $dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(rawLine);
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
		return {aa: body, dJ: language};
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
	var _v0 = fenceCharacter.aI;
	if (!_v0) {
		return A2(
			$elm$parser$Parser$Advanced$mapChompedString,
			toInfoString,
			$elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return (c !== '`') && (!$dillonkearns$elm_markdown$Whitespace$isLineEnd(c));
				}));
	} else {
		return A2(
			$elm$parser$Parser$Advanced$mapChompedString,
			toInfoString,
			$elm$parser$Parser$Advanced$chompWhile(
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $dillonkearns$elm_markdown$Whitespace$isLineEnd)));
	}
};
var $dillonkearns$elm_markdown$Markdown$CodeBlock$Backtick = 0;
var $dillonkearns$elm_markdown$Parser$Token$backtick = A2(
	$elm$parser$Parser$Advanced$Token,
	'`',
	$elm$parser$Parser$Expecting('a \'`\''));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$backtick = {ax: '`', aI: 0, aO: $dillonkearns$elm_markdown$Parser$Token$backtick};
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
			$elm$parser$Parser$Advanced$succeed(0),
			A2(
				$elm$core$List$repeat,
				minLength,
				$elm$parser$Parser$Advanced$token(fenceCharacter.aO)));
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
					$elm$core$Basics$eq(fenceCharacter.ax))));
	});
var $elm$parser$Parser$Advanced$getCol = function (s) {
	return A3($elm$parser$Parser$Advanced$Good, false, s.bq, s);
};
var $dillonkearns$elm_markdown$Markdown$CodeBlock$Tilde = 1;
var $dillonkearns$elm_markdown$Parser$Token$tilde = A2(
	$elm$parser$Parser$Advanced$Token,
	'~',
	$elm$parser$Parser$Expecting('a `~`'));
var $dillonkearns$elm_markdown$Markdown$CodeBlock$tilde = {ax: '~', aI: 1, aO: $dillonkearns$elm_markdown$Parser$Token$tilde};
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
							$elm$parser$Parser$Advanced$succeed(0)
						]))),
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						$dillonkearns$elm_markdown$Whitespace$space,
						$elm$parser$Parser$Advanced$succeed(0)
					]))),
			$elm$parser$Parser$Advanced$succeed(0)
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
						return {ay: character, a$: indent, a1: length};
					})),
			$dillonkearns$elm_markdown$Whitespace$upToThreeSpaces),
		A2($elm$parser$Parser$Advanced$andThen, $dillonkearns$elm_markdown$Markdown$CodeBlock$colToIndentation, $elm$parser$Parser$Advanced$getCol)),
	$elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2($dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast, 3, $dillonkearns$elm_markdown$Markdown$CodeBlock$backtick),
				A2($dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast, 3, $dillonkearns$elm_markdown$Markdown$CodeBlock$tilde)
			])));
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $dillonkearns$elm_markdown$Whitespace$isSpace = $elm$core$Basics$eq(' ');
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
						$elm$parser$Parser$Advanced$succeed(0),
						$dillonkearns$elm_markdown$Whitespace$upToThreeSpaces),
					A2($dillonkearns$elm_markdown$Markdown$CodeBlock$fenceOfAtLeast, minLength, fenceCharacter)),
				$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpace)),
			$dillonkearns$elm_markdown$Helpers$lineEndOrEnd);
	});
var $dillonkearns$elm_markdown$Parser$Extra$upTo = F2(
	function (n, parser) {
		var _v0 = A2($elm$core$List$repeat, n, parser);
		if (!_v0.b) {
			return $elm$parser$Parser$Advanced$succeed(0);
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
									$elm$parser$Parser$Advanced$succeed(0)
								]));
					}),
				$elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							firstParser,
							$elm$parser$Parser$Advanced$succeed(0)
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
var $elm$parser$Parser$Advanced$getSource = function (s) {
	return A3($elm$parser$Parser$Advanced$Good, false, s.ba, s);
};
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
					A2($dillonkearns$elm_markdown$Markdown$CodeBlock$closingFence, fence.a1, fence.ay))),
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
						$dillonkearns$elm_markdown$Markdown$CodeBlock$codeBlockLine(fence.a$)),
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
					$dillonkearns$elm_markdown$Markdown$CodeBlock$infoString(fence.ay),
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
	if ('#' === c) {
		return true;
	} else {
		return false;
	}
};
var $elm$parser$Parser$Advanced$spaces = $elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return (c === ' ') || ((c === '\n') || (c === '\r'));
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
				$elm$parser$Parser$Advanced$succeed(''),
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
							return $dillonkearns$elm_markdown$Markdown$Heading$dropClosingSequence(
								$elm$core$String$trim(headingText));
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
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$findSubString, str, s.b, s.ek, s.bq, s.ba);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A4($elm$parser$Parser$Advanced$fromInfo, newRow, newCol, expecting, s.e)) : A3(
			$elm$parser$Parser$Advanced$Good,
			_Utils_cmp(s.b, newOffset) < 0,
			0,
			{bq: newCol, e: s.e, g: s.g, b: newOffset, ek: newRow, ba: s.ba});
	};
};
var $dillonkearns$elm_markdown$Parser$Token$greaterThan = A2(
	$elm$parser$Parser$Advanced$Token,
	'>',
	$elm$parser$Parser$Expecting('a `>`'));
var $elm$parser$Parser$Advanced$Located = F3(
	function (row, col, context) {
		return {bq: col, e: context, ek: row};
	});
var $elm$parser$Parser$Advanced$changeContext = F2(
	function (newContext, s) {
		return {bq: s.bq, e: newContext, g: s.g, b: s.b, ek: s.ek, ba: s.ba};
	});
var $elm$parser$Parser$Advanced$inContext = F2(
	function (context, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(
				A2(
					$elm$parser$Parser$Advanced$changeContext,
					A2(
						$elm$core$List$cons,
						A3($elm$parser$Parser$Advanced$Located, s0.ek, s0.bq, context),
						s0.e),
					s0));
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					a,
					A2($elm$parser$Parser$Advanced$changeContext, s0.e, s1));
			} else {
				var step = _v1;
				return step;
			}
		};
	});
var $dillonkearns$elm_markdown$Whitespace$isWhitespace = function (_char) {
	switch (_char) {
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
									{c8: destination, cf: title});
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
									$elm$parser$Parser$Advanced$succeed(0)
								]))),
					$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab))),
			$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$destinationParser),
		$dillonkearns$elm_markdown$Markdown$LinkReferenceDefinition$titleParser));
var $dillonkearns$elm_markdown$ThematicBreak$ThematicBreak = 0;
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
								$elm$parser$Parser$Advanced$succeed(0),
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
			$dillonkearns$elm_markdown$ThematicBreak$withChar('-'),
			$dillonkearns$elm_markdown$ThematicBreak$withChar('*'),
			$dillonkearns$elm_markdown$ThematicBreak$withChar('_')
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
								$elm$parser$Parser$Advanced$succeed(0)
							]))),
				$elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							$dillonkearns$elm_markdown$Whitespace$space,
							$elm$parser$Parser$Advanced$succeed(0)
						]))),
			$dillonkearns$elm_markdown$ThematicBreak$parseThematicBreak),
			$dillonkearns$elm_markdown$ThematicBreak$parseThematicBreak
		]));
var $dillonkearns$elm_markdown$Markdown$RawBlock$LevelOne = 0;
var $dillonkearns$elm_markdown$Markdown$RawBlock$LevelTwo = 1;
var $dillonkearns$elm_markdown$Markdown$RawBlock$SetextLine = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
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
								A3(setextLevel, 0, $dillonkearns$elm_markdown$Parser$Token$equals, '='),
								A3(setextLevel, 1, $dillonkearns$elm_markdown$Parser$Token$minus, '-')
							])),
					$elm$parser$Parser$Advanced$chompWhile($dillonkearns$elm_markdown$Whitespace$isSpaceOrTab)),
				$dillonkearns$elm_markdown$Helpers$lineEndOrEnd)));
}();
var $dillonkearns$elm_markdown$Markdown$RawBlock$TableDelimiter = function (a) {
	return {$: 9, a: a};
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
				$elm$parser$Parser$Advanced$succeed(0)
			]));
};
var $dillonkearns$elm_markdown$Markdown$TableParser$requirePipeIfNotFirst = function (columns) {
	return $elm$core$List$isEmpty(columns) ? $elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				$dillonkearns$elm_markdown$Parser$Token$parseString('|'),
				$elm$parser$Parser$Advanced$succeed(0)
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
									$elm$parser$Parser$Advanced$succeed(0),
									$dillonkearns$elm_markdown$Parser$Extra$maybeChomp(
										function (c) {
											return c === ':';
										})),
								$dillonkearns$elm_markdown$Parser$Extra$chompOneOrMore(
									function (c) {
										return c === '-';
									})),
							$dillonkearns$elm_markdown$Parser$Extra$maybeChomp(
								function (c) {
									return c === ':';
								}))),
					$dillonkearns$elm_markdown$Markdown$TableParser$chompSinglelineWhitespace))
			]));
};
var $dillonkearns$elm_markdown$Markdown$Block$AlignCenter = 2;
var $dillonkearns$elm_markdown$Markdown$Block$AlignLeft = 0;
var $dillonkearns$elm_markdown$Markdown$Block$AlignRight = 1;
var $dillonkearns$elm_markdown$Markdown$TableParser$delimiterToAlignment = function (cell) {
	var _v0 = _Utils_Tuple2(
		A2($elm$core$String$startsWith, ':', cell),
		A2($elm$core$String$endsWith, ':', cell));
	if (_v0.a) {
		if (_v0.b) {
			return $elm$core$Maybe$Just(2);
		} else {
			return $elm$core$Maybe$Just(0);
		}
	} else {
		if (_v0.b) {
			return $elm$core$Maybe$Just(1);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}
};
var $dillonkearns$elm_markdown$Markdown$TableParser$delimiterRowParser = A2(
	$elm$parser$Parser$Advanced$andThen,
	function (delimiterRow) {
		var trimmed = delimiterRow.a.ci;
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
						b1: delimiterText,
						ci: $elm$core$String$trim(delimiterText)
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
var $dillonkearns$elm_markdown$Markdown$TableParser$standardizeRowLength = F2(
	function (expectedLength, row) {
		var rowLength = $elm$core$List$length(row);
		var _v0 = A2($elm$core$Basics$compare, expectedLength, rowLength);
		switch (_v0) {
			case 0:
				return A2($elm$core$List$take, expectedLength, row);
			case 1:
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
var $dillonkearns$elm_markdown$Markdown$Block$H1 = 0;
var $dillonkearns$elm_markdown$Markdown$Block$H2 = 1;
var $dillonkearns$elm_markdown$Markdown$Block$H3 = 2;
var $dillonkearns$elm_markdown$Markdown$Block$H4 = 3;
var $dillonkearns$elm_markdown$Markdown$Block$H5 = 4;
var $dillonkearns$elm_markdown$Markdown$Block$H6 = 5;
var $dillonkearns$elm_markdown$Markdown$Parser$toHeading = function (level) {
	switch (level) {
		case 1:
			return $elm$core$Result$Ok(0);
		case 2:
			return $elm$core$Result$Ok(1);
		case 3:
			return $elm$core$Result$Ok(2);
		case 4:
			return $elm$core$Result$Ok(3);
		case 5:
			return $elm$core$Result$Ok(4);
		case 6:
			return $elm$core$Result$Ok(5);
		default:
			return $elm$core$Result$Err(
				$elm$parser$Parser$Expecting(
					'A heading with 1 to 6 #\'s, but found ' + $elm$core$String$fromInt(level)));
	}
};
var $dillonkearns$elm_markdown$Markdown$RawBlock$UnorderedListBlock = function (a) {
	return {$: 3, a: a};
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
	return {$: 1, a: a};
};
var $dillonkearns$elm_markdown$Markdown$ListItem$TaskItem = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $dillonkearns$elm_markdown$Markdown$ListItem$Complete = 1;
var $dillonkearns$elm_markdown$Markdown$ListItem$Incomplete = 0;
var $dillonkearns$elm_markdown$Markdown$ListItem$taskItemParser = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(1),
			$elm$parser$Parser$Advanced$symbol(
				A2(
					$elm$parser$Parser$Advanced$Token,
					'[x] ',
					$elm$parser$Parser$ExpectingSymbol('[x] ')))),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(1),
			$elm$parser$Parser$Advanced$symbol(
				A2(
					$elm$parser$Parser$Advanced$Token,
					'[X] ',
					$elm$parser$Parser$ExpectingSymbol('[X] ')))),
			A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(0),
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
		if (!unparsedListItem.$) {
			var completion = unparsedListItem.a;
			var body = unparsedListItem.b;
			return {
				aa: body,
				bd: $elm$core$Maybe$Just(
					function () {
						if (completion === 1) {
							return true;
						} else {
							return false;
						}
					}())
			};
		} else {
			var body = unparsedListItem.a;
			return {aa: body, bd: $elm$core$Maybe$Nothing};
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
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $dillonkearns$elm_markdown$Markdown$Parser$childToBlocks = F2(
	function (node, blocks) {
		switch (node.$) {
			case 0:
				var tag = node.a;
				var attributes = node.b;
				var children = node.c;
				var _v28 = $dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocks(children);
				if (!_v28.$) {
					var childrenAsBlocks = _v28.a;
					var block = $dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
						A3($dillonkearns$elm_markdown$Markdown$Block$HtmlElement, tag, attributes, childrenAsBlocks));
					return $elm$core$Result$Ok(
						A2($elm$core$List$cons, block, blocks));
				} else {
					var err = _v28.a;
					return $elm$core$Result$Err(err);
				}
			case 1:
				var innerText = node.a;
				var _v29 = $dillonkearns$elm_markdown$Markdown$Parser$parse(innerText);
				if (!_v29.$) {
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
			case 2:
				var string = node.a;
				return $elm$core$Result$Ok(
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							$dillonkearns$elm_markdown$Markdown$Block$HtmlComment(string)),
						blocks));
			case 3:
				var string = node.a;
				return $elm$core$Result$Ok(
					A2(
						$elm$core$List$cons,
						$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(
							$dillonkearns$elm_markdown$Markdown$Block$Cdata(string)),
						blocks));
			case 4:
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
		var unparsedInlines = _v23;
		var mappedReferencesDict = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$mapSecond(
					function (_v24) {
						var destination = _v24.c8;
						var title = _v24.cf;
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
		case 0:
			var string = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$Text(string);
		case 1:
			return $dillonkearns$elm_markdown$Markdown$Block$HardLineBreak;
		case 2:
			var string = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$CodeSpan(string);
		case 3:
			var string = inline.a;
			var maybeString = inline.b;
			var inlines = inline.c;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Block$Link,
				string,
				maybeString,
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
		case 4:
			var string = inline.a;
			var maybeString = inline.b;
			var inlines = inline.c;
			return A3(
				$dillonkearns$elm_markdown$Markdown$Block$Image,
				string,
				maybeString,
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Parser$mapInline, inlines));
		case 5:
			var node = inline.a;
			return $dillonkearns$elm_markdown$Markdown$Block$HtmlInline(
				$dillonkearns$elm_markdown$Markdown$Parser$nodeToRawBlock(node));
		case 6:
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
		case 1:
			var innerText = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$HtmlComment('TODO this never happens, but use types to drop this case.');
		case 0:
			var tag = node.a;
			var attributes = node.b;
			var children = node.c;
			var parseChild = function (child) {
				if (child.$ === 1) {
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
		case 2:
			var string = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$HtmlComment(string);
		case 3:
			var string = node.a;
			return $dillonkearns$elm_markdown$Markdown$Block$Cdata(string);
		case 4:
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
				if (!_v18.$) {
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
	if (_v14.$ === 1) {
		var e = _v14.a;
		return $elm$core$Result$Err(e);
	} else {
		var v = _v14.a;
		var _v15 = $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlines(v);
		if (_v15.$ === 1) {
			var e = _v15.a;
			return A2(
				$elm$parser$Parser$Advanced$run,
				$elm$parser$Parser$Advanced$problem(e),
				'');
		} else {
			var blocks = _v15.a;
			var isNotEmptyParagraph = function (block) {
				if ((block.$ === 5) && (!block.a.b)) {
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
	return A3($dillonkearns$elm_markdown$Markdown$Parser$parseAllInlinesHelp, state, state.G, _List_Nil);
};
var $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlinesHelp = F3(
	function (state, rawBlocks, parsedBlocks) {
		parseAllInlinesHelp:
		while (true) {
			if (rawBlocks.b) {
				var rawBlock = rawBlocks.a;
				var rest = rawBlocks.b;
				var _v13 = A2($dillonkearns$elm_markdown$Markdown$Parser$parseInlines, state.T, rawBlock);
				switch (_v13.$) {
					case 1:
						var newParsedBlock = _v13.a;
						var $temp$state = state,
							$temp$rawBlocks = rest,
							$temp$parsedBlocks = A2($elm$core$List$cons, newParsedBlock, parsedBlocks);
						state = $temp$state;
						rawBlocks = $temp$rawBlocks;
						parsedBlocks = $temp$parsedBlocks;
						continue parseAllInlinesHelp;
					case 0:
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
				var label = _v11.a0;
				var alignment = _v11.au;
				return A3(
					$dillonkearns$elm_markdown$Markdown$Parser$parseRawInline,
					linkReferences,
					function (parsedHeaderLabel) {
						return {au: alignment, a0: parsedHeaderLabel};
					},
					label);
			},
			header);
	});
var $dillonkearns$elm_markdown$Markdown$Parser$parseInlines = F2(
	function (linkReferences, rawBlock) {
		switch (rawBlock.$) {
			case 0:
				var level = rawBlock.a;
				var unparsedInlines = rawBlock.b;
				var _v5 = $dillonkearns$elm_markdown$Markdown$Parser$toHeading(level);
				if (!_v5.$) {
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
			case 1:
				var unparsedInlines = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$Paragraph(
						A2($dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper, linkReferences, unparsedInlines)));
			case 2:
				var html = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$HtmlBlock(html));
			case 3:
				var unparsedItems = rawBlock.a;
				var parseItem = function (unparsed) {
					var task = function () {
						var _v6 = unparsed.bd;
						if (!_v6.$) {
							if (!_v6.a) {
								return 1;
							} else {
								return 2;
							}
						} else {
							return 0;
						}
					}();
					var parsedInlines = A3($dillonkearns$elm_markdown$Markdown$Parser$parseRawInline, linkReferences, $elm$core$Basics$identity, unparsed.aa);
					return A2($dillonkearns$elm_markdown$Markdown$Block$ListItem, task, parsedInlines);
				};
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$UnorderedList(
						A2($elm$core$List$map, parseItem, unparsedItems)));
			case 4:
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
			case 5:
				var codeBlock = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$CodeBlock(codeBlock));
			case 7:
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock($dillonkearns$elm_markdown$Markdown$Block$ThematicBreak);
			case 10:
				return $dillonkearns$elm_markdown$Markdown$Parser$EmptyBlock;
			case 11:
				var rawBlocks = rawBlock.a;
				var _v7 = A2(
					$elm$parser$Parser$Advanced$run,
					$dillonkearns$elm_markdown$Markdown$Parser$cyclic$rawBlockParser(),
					rawBlocks);
				if (!_v7.$) {
					var value = _v7.a;
					var _v8 = $dillonkearns$elm_markdown$Markdown$Parser$parseAllInlines(value);
					if (!_v8.$) {
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
			case 6:
				var codeBlockBody = rawBlock.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$CodeBlock(
						{aa: codeBlockBody, dJ: $elm$core$Maybe$Nothing}));
			case 8:
				var _v9 = rawBlock.a;
				var header = _v9.a;
				var rows = _v9.b;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					A2(
						$dillonkearns$elm_markdown$Markdown$Block$Table,
						A2($dillonkearns$elm_markdown$Markdown$Parser$parseHeaderInlines, linkReferences, header),
						A2($dillonkearns$elm_markdown$Markdown$Parser$parseRowInlines, linkReferences, rows)));
			case 9:
				var _v10 = rawBlock.a;
				var text = _v10.a;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$Paragraph(
						A2($dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper, linkReferences, text.b1)));
			default:
				var raw = rawBlock.b;
				return $dillonkearns$elm_markdown$Markdown$Parser$ParsedBlock(
					$dillonkearns$elm_markdown$Markdown$Block$Paragraph(
						A2($dillonkearns$elm_markdown$Markdown$Parser$inlineParseHelper, linkReferences, raw)));
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
						return A3($dillonkearns$elm_markdown$Markdown$Parser$parseRawInline, linkReferences, $elm$core$Basics$identity, column);
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
					var _v3 = revStmts.G;
					_v3$2:
					while (true) {
						if (_v3.b) {
							switch (_v3.a.$) {
								case 1:
									return $dillonkearns$elm_markdown$Markdown$Parser$cyclic$mergeableBlockAfterOpenBlockOrParagraphParser();
								case 8:
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
		case 1:
			var innerText = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$OpenBlockOrParagraph(innerText));
		case 0:
			var tag = xmlNode.a;
			var attributes = xmlNode.b;
			var children = xmlNode.c;
			var _v1 = $dillonkearns$elm_markdown$Markdown$Parser$nodesToBlocks(children);
			if (!_v1.$) {
				var parsedChildren = _v1.a;
				return $elm$parser$Parser$Advanced$succeed(
					$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
						A3($dillonkearns$elm_markdown$Markdown$Block$HtmlElement, tag, attributes, parsedChildren)));
			} else {
				var err = _v1.a;
				return $elm$parser$Parser$Advanced$problem(err);
			}
		case 2:
			var string = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
					$dillonkearns$elm_markdown$Markdown$Block$HtmlComment(string)));
		case 3:
			var string = xmlNode.a;
			return $elm$parser$Parser$Advanced$succeed(
				$dillonkearns$elm_markdown$Markdown$RawBlock$Html(
					$dillonkearns$elm_markdown$Markdown$Block$Cdata(string)));
		case 4:
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
		{T: _List_Nil, G: _List_Nil},
		$dillonkearns$elm_markdown$Markdown$Parser$stepRawBlock);
}
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
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 1) {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 1) {
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
					case 0:
						var html = block.a;
						if (!html.$) {
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
					case 1:
						var listItems = block.a;
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 2:
						var _int = block.a;
						var lists = block.b;
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 3:
						var blocks = block.a;
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = _Utils_ap(blocks, remainingBlocks);
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 4:
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 5:
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 6:
						var $temp$function = _function,
							$temp$acc = A2(_function, block, acc),
							$temp$list = remainingBlocks;
						_function = $temp$function;
						acc = $temp$acc;
						list = $temp$list;
						continue foldl;
					case 7:
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
		case 5:
			var inlines = block.a;
			return $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines);
		case 0:
			var html = block.a;
			if (!html.$) {
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
		case 1:
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
		case 2:
			var _int = block.a;
			var items = block.b;
			return A2(
				$elm$core$String$join,
				'\n',
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Block$extractInlineText, items));
		case 3:
			var blocks = block.a;
			return A2(
				$elm$core$String$join,
				'\n',
				A2($elm$core$List$map, $dillonkearns$elm_markdown$Markdown$Block$extractInlineBlockText, blocks));
		case 4:
			var headingLevel = block.a;
			var inlines = block.b;
			return $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines);
		case 6:
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
									return $.a0;
								},
								header)),
							$elm$core$List$concat(
							A2(
								$elm$core$List$map,
								$elm$core$List$map($dillonkearns$elm_markdown$Markdown$Block$extractInlineText),
								rows))
						])));
		case 7:
			var body = block.a.aa;
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
			case 7:
				var str = inline.a;
				return _Utils_ap(text, str);
			case 8:
				return text + ' ';
			case 6:
				var str = inline.a;
				return _Utils_ap(text, str);
			case 1:
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			case 2:
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			case 0:
				var html = inline.a;
				if (!html.$) {
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
			case 4:
				var inlines = inline.a;
				return _Utils_ap(
					text,
					$dillonkearns$elm_markdown$Markdown$Block$extractInlineText(inlines));
			case 3:
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
		var htmlRenderer = _v0;
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
		if (!_v7.$) {
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
			case 4:
				var level = block.a;
				var content = block.b;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						function (children) {
							return renderer.dl(
								{
									cZ: children,
									dP: level,
									ec: $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(content)
								});
						},
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, content)));
			case 5:
				var content = block.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.d7,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, content)));
			case 0:
				var html = block.a;
				if (!html.$) {
					var tag = html.a;
					var attributes = html.b;
					var children = html.c;
					return $elm$core$Maybe$Just(
						A4($dillonkearns$elm_markdown$Markdown$Renderer$renderHtmlNode, renderer, tag, attributes, children));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			case 1:
				var items = block.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.fb,
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
			case 2:
				var startingIndex = block.a;
				var items = block.b;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.d4(startingIndex),
						$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
							A2(
								$elm$core$List$map,
								$dillonkearns$elm_markdown$Markdown$Renderer$renderStyled(renderer),
								items))));
			case 7:
				var codeBlock = block.a;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.c1(codeBlock)));
			case 8:
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(renderer.e3));
			case 3:
				var nestedBlocks = block.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.cK,
						$dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
							A2($dillonkearns$elm_markdown$Markdown$Renderer$renderHelper, renderer, nestedBlocks))));
			default:
				var header = block.a;
				var rows = block.b;
				var renderedHeaderCells = $dillonkearns$elm_markdown$Markdown$Renderer$combineResults(
					A2(
						$elm$core$List$map,
						function (_v6) {
							var label = _v6.a0;
							var alignment = _v6.au;
							return A2(
								$elm$core$Result$map,
								$elm$core$Tuple$pair(alignment),
								A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, label));
						},
						header));
				var renderedHeader = A2(
					$elm$core$Result$map,
					function (listListView) {
						return renderer.eN(
							$elm$core$List$singleton(
								renderer.eP(
									A2(
										$elm$core$List$map,
										function (_v5) {
											var maybeAlignment = _v5.a;
											var item = _v5.b;
											return A2(renderer.eO, maybeAlignment, item);
										},
										listListView))));
					},
					renderedHeaderCells);
				var renderedBody = function (r) {
					return $elm$core$List$isEmpty(r) ? _List_Nil : _List_fromArray(
						[
							renderer.eL(r)
						]);
				};
				var alignmentForColumn = function (columnIndex) {
					return A2(
						$elm$core$Maybe$andThen,
						function ($) {
							return $.au;
						},
						$elm$core$List$head(
							A2($elm$core$List$drop, columnIndex, header)));
				};
				var renderRow = function (cells) {
					return A2(
						$elm$core$Result$map,
						renderer.eP,
						A2(
							$elm$core$Result$map,
							$elm$core$List$indexedMap(
								F2(
									function (index, cell) {
										return A2(
											renderer.eM,
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
								return renderer.eK(
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
			renderer.$7,
			A2($dillonkearns$elm_markdown$Markdown$Renderer$renderHelper, renderer, children));
	});
var $dillonkearns$elm_markdown$Markdown$Renderer$renderSingleInline = F2(
	function (renderer, inline) {
		switch (inline.$) {
			case 4:
				var innerInlines = inline.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.eH,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, innerInlines)));
			case 3:
				var innerInlines = inline.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.dd,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, innerInlines)));
			case 5:
				var innerInlines = inline.a;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$map,
						renderer.eG,
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, innerInlines)));
			case 2:
				var src = inline.a;
				var title = inline.b;
				var children = inline.c;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.dr(
							{
								cC: $dillonkearns$elm_markdown$Markdown$Block$extractInlineText(children),
								ba: src,
								cf: title
							})));
			case 7:
				var string = inline.a;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.eR(string)));
			case 6:
				var string = inline.a;
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(
						renderer.c2(string)));
			case 1:
				var destination = inline.a;
				var title = inline.b;
				var inlines = inline.c;
				return $elm$core$Maybe$Just(
					A2(
						$elm$core$Result$andThen,
						function (children) {
							return $elm$core$Result$Ok(
								A2(
									renderer.dQ,
									{c8: destination, cf: title},
									children));
						},
						A2($dillonkearns$elm_markdown$Markdown$Renderer$renderStyled, renderer, inlines)));
			case 8:
				return $elm$core$Maybe$Just(
					$elm$core$Result$Ok(renderer.dj));
			default:
				var html = inline.a;
				if (!html.$) {
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
var $mdgriffith$elm_ui$Internal$Model$Top = 0;
var $mdgriffith$elm_ui$Element$alignTop = $mdgriffith$elm_ui$Internal$Model$AlignY(0);
var $elm$html$Html$br = _VirtualDom_node('br');
var $mdgriffith$elm_ui$Internal$Flag$fontAlignment = $mdgriffith$elm_ui$Internal$Flag$flag(12);
var $mdgriffith$elm_ui$Element$Font$center = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.eS);
var $mdgriffith$elm_ui$Internal$Model$ImportFont = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$Font$external = function (_v0) {
	var url = _v0.cj;
	var name = _v0.bP;
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
						{bP: 'Source Code Pro', cj: 'https://fonts.googleapis.com/css?family=Source+Code+Pro'})
					]))
			]),
		$mdgriffith$elm_ui$Element$text(snippet));
};
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
						{bP: 'Source Code Pro', cj: 'https://fonts.googleapis.com/css?family=Source+Code+Pro'})
					]))
			]),
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$text(details.aa)
			]));
};
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $mdgriffith$elm_ui$Internal$Model$Rotate = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
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
				shadow.bG ? 'box-inset' : 'box-',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.b.a) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.b.b) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow._) + 'px',
				$mdgriffith$elm_ui$Internal$Model$floatClass(shadow.ca) + 'px',
				$mdgriffith$elm_ui$Internal$Model$formatColorClass(shadow.ab)
			]));
};
var $mdgriffith$elm_ui$Internal$Flag$shadows = $mdgriffith$elm_ui$Internal$Flag$flag(19);
var $mdgriffith$elm_ui$Element$Border$shadow = function (almostShade) {
	var shade = {_: almostShade._, ab: almostShade.ab, bG: false, b: almostShade.b, ca: almostShade.ca};
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$shadows,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			$mdgriffith$elm_ui$Internal$Model$boxShadowClass(shade),
			'box-shadow',
			$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(shade)));
};
var $mdgriffith$elm_ui$Element$transparent = function (on) {
	return on ? A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2($mdgriffith$elm_ui$Internal$Model$Transparency, 'transparent', 1.0)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2($mdgriffith$elm_ui$Internal$Model$Transparency, 'visible', 0.0));
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
	var bottom = _v0.cQ;
	var top = _v0.e5;
	var left = _v0.dK;
	var right = _v0.eh;
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
					_: 1,
					ab: checked ? A4($mdgriffith$elm_ui$Element$rgba, 238 / 255, 238 / 255, 238 / 255, 0) : A3($mdgriffith$elm_ui$Element$rgb, 238 / 255, 238 / 255, 238 / 255),
					b: _Utils_Tuple2(0, 0),
					ca: 1
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
							{cQ: 2, dK: 2, eh: 0, e5: 0})
						]),
					$mdgriffith$elm_ui$Element$none))
			]),
		$mdgriffith$elm_ui$Element$none);
};
var $mdgriffith$elm_ui$Internal$Model$Heading = function (a) {
	return {$: 4, a: a};
};
var $mdgriffith$elm_ui$Element$Region$heading = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Describe, $mdgriffith$elm_ui$Internal$Model$Heading);
var $dillonkearns$elm_markdown$Markdown$Block$headingLevelToInt = function (headingLevel) {
	switch (headingLevel) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
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
	var level = _v0.dP;
	var rawText = _v0.ec;
	var children = _v0.cZ;
	return A2(
		$mdgriffith$elm_ui$Element$paragraph,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Font$size(
				function () {
					switch (level) {
						case 0:
							return 36;
						case 1:
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
var $elm$html$Html$Attributes$alt = $elm$html$Html$Attributes$stringProperty('alt');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $mdgriffith$elm_ui$Element$image = F2(
	function (attrs, _v0) {
		var src = _v0.ba;
		var description = _v0.bs;
		var imageAttributes = A2(
			$elm$core$List$filter,
			function (a) {
				switch (a.$) {
					case 7:
						return true;
					case 8:
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
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ds),
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
var $dillonkearns$elm_markdown$Markdown$HtmlRenderer$HtmlRenderer = $elm$core$Basics$identity;
var $dillonkearns$elm_markdown$Markdown$Html$resultOr = F2(
	function (ra, rb) {
		if (ra.$ === 1) {
			var singleError = ra.a;
			if (!rb.$) {
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
				var name = _v0.bP;
				var value = _v0.bg;
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
			var rawDecoder = _v1;
			return rawDecoder;
		},
		decoders);
	return function (rawDecoder) {
		return F3(
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
			});
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
var $author$project$Utils$Markdown$renderEmojiTag = F2(
	function (emojiName, rendererdChildren) {
		return A2(
			$mdgriffith$elm_ui$Element$image,
			_List_Nil,
			{
				bs: '',
				ba: $author$project$Emoji$hexToPath(emojiName)
			});
	});
var $mdgriffith$elm_ui$Element$Font$strike = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.eF);
var $mdgriffith$elm_ui$Internal$Flag$borderStyle = $mdgriffith$elm_ui$Internal$Flag$flag(11);
var $mdgriffith$elm_ui$Element$Border$solid = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$borderStyle, $mdgriffith$elm_ui$Internal$Style$classes.cP);
var $author$project$Utils$Markdown$tableBorder = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Border$color(
		A3($mdgriffith$elm_ui$Element$rgb255, 223, 226, 229)),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$Border$solid,
		A2($mdgriffith$elm_ui$Element$paddingXY, 6, 13),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
	]);
var $dillonkearns$elm_markdown$Markdown$Html$tag = F2(
	function (expectedTag, a) {
		return F3(
			function (tagName, attributes, children) {
				return _Utils_eq(tagName, expectedTag) ? $elm$core$Result$Ok(a) : $elm$core$Result$Err('Expected ' + (expectedTag + (' but was ' + tagName)));
			});
	});
var $dillonkearns$elm_markdown$List$Helpers$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $dillonkearns$elm_markdown$Markdown$Html$withAttribute = F2(
	function (attributeName, _v0) {
		var renderer = _v0;
		return F3(
			function (tagName, attributes, innerBlocks) {
				return function () {
					var _v1 = A2(
						$dillonkearns$elm_markdown$List$Helpers$find,
						function (_v2) {
							var name = _v2.bP;
							var value = _v2.bg;
							return _Utils_eq(name, attributeName);
						},
						attributes);
					if (!_v1.$) {
						var value = _v1.a.bg;
						return $elm$core$Result$map(
							$elm$core$Basics$apR(value));
					} else {
						return function (_v3) {
							return $elm$core$Result$Err('Expecting attribute \"' + (attributeName + '\".'));
						};
					}
				}()(
					A3(renderer, tagName, attributes, innerBlocks));
			});
	});
var $author$project$Utils$Markdown$renderer = {
	cK: function (children) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Border$widthEach(
					{cQ: 0, dK: 10, eh: 0, e5: 0}),
					$mdgriffith$elm_ui$Element$padding(10),
					$mdgriffith$elm_ui$Element$Border$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 145, 145, 145)),
					$mdgriffith$elm_ui$Element$Background$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 245, 245, 245))
				]),
			children);
	},
	c1: $author$project$Utils$Markdown$codeBlock,
	c2: $author$project$Utils$Markdown$code,
	dd: function (content) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$Font$italic]),
			content);
	},
	dj: $mdgriffith$elm_ui$Element$html(
		A2($elm$html$Html$br, _List_Nil, _List_Nil)),
	dl: $author$project$Utils$Markdown$heading,
	$7: $dillonkearns$elm_markdown$Markdown$Html$oneOf(
		_List_fromArray(
			[
				A2(
				$dillonkearns$elm_markdown$Markdown$Html$withAttribute,
				'name',
				A2($dillonkearns$elm_markdown$Markdown$Html$tag, 'emoji', $author$project$Utils$Markdown$renderEmojiTag))
			])),
	dr: function (image) {
		return A2(
			$mdgriffith$elm_ui$Element$image,
			_List_Nil,
			{bs: image.cC, ba: image.ba});
	},
	dQ: F2(
		function (_v0, body) {
			var title = _v0.cf;
			var destination = _v0.c8;
			return A2(
				$mdgriffith$elm_ui$Element$newTabLink,
				_List_Nil,
				{
					a0: A2(
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
					cj: destination
				});
		}),
	d4: F2(
		function (startingIndex, items) {
			return A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$spacing(10)
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
	d7: $mdgriffith$elm_ui$Element$paragraph(
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 5)
			])),
	eG: function (content) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$Font$strike]),
			content);
	},
	eH: function (content) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$Font$bold]),
			content);
	},
	eK: $mdgriffith$elm_ui$Element$column(_List_Nil),
	eL: $mdgriffith$elm_ui$Element$column(_List_Nil),
	eM: F2(
		function (maybeAlignment, children) {
			return A2($mdgriffith$elm_ui$Element$paragraph, $author$project$Utils$Markdown$tableBorder, children);
		}),
	eN: $mdgriffith$elm_ui$Element$column(
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Font$bold,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$Font$center
			])),
	eO: F2(
		function (maybeAlignment, children) {
			return A2($mdgriffith$elm_ui$Element$paragraph, $author$project$Utils$Markdown$tableBorder, children);
		}),
	eP: $mdgriffith$elm_ui$Element$row(
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
			])),
	eR: function (value) {
		return A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_Nil,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$text(value)
				]));
	},
	e3: $mdgriffith$elm_ui$Element$none,
	fb: function (items) {
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(10)
				]),
			A2(
				$elm$core$List$map,
				function (_v1) {
					var task = _v1.a;
					var children = _v1.b;
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
										switch (task) {
											case 1:
												return $mdgriffith$elm_ui$Element$Input$defaultCheckbox(false);
											case 2:
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
			A2(
				$elm$core$Result$map,
				$author$project$Utils$Markdown$handleEmojis_Blocks,
				$dillonkearns$elm_markdown$Markdown$Parser$parse(str))));
	if (result.$ === 1) {
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
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $author$project$Utils$Utils$monthToInt = function (month) {
	switch (month) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 8;
		case 8:
			return 9;
		case 9:
			return 10;
		case 10:
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.h, posixMinutes) < 0) {
					return posixMinutes + era.b;
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
		br: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		bM: month,
		cp: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).br;
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
var $elm$time$Time$Apr = 3;
var $elm$time$Time$Aug = 7;
var $elm$time$Time$Dec = 11;
var $elm$time$Time$Feb = 1;
var $elm$time$Time$Jan = 0;
var $elm$time$Time$Jul = 6;
var $elm$time$Time$Jun = 5;
var $elm$time$Time$Mar = 2;
var $elm$time$Time$May = 4;
var $elm$time$Time$Nov = 10;
var $elm$time$Time$Oct = 9;
var $elm$time$Time$Sep = 8;
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).bM;
		switch (_v0) {
			case 1:
				return 0;
			case 2:
				return 1;
			case 3:
				return 2;
			case 4:
				return 3;
			case 5:
				return 4;
			case 6:
				return 5;
			case 7:
				return 6;
			case 8:
				return 7;
			case 9:
				return 8;
			case 10:
				return 9;
			case 11:
				return 10;
			default:
				return 11;
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
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).cp;
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $author$project$Utils$Utils$formatTime = F2(
	function (targetPosix, currentPosix) {
		var year = A2($elm$time$Time$toYear, $elm$time$Time$utc, targetPosix);
		var sec = A2($elm$time$Time$toSecond, $elm$time$Time$utc, targetPosix);
		var prefixZero = function (n) {
			return ($elm$core$Basics$abs(n) < 10) ? ('0' + $elm$core$String$fromInt(n)) : $elm$core$String$fromInt(n);
		};
		var month = $author$project$Utils$Utils$monthToInt(
			A2($elm$time$Time$toMonth, $elm$time$Time$utc, targetPosix));
		var min = A2($elm$time$Time$toMinute, $elm$time$Time$utc, targetPosix);
		var hour = A2($elm$time$Time$toHour, $elm$time$Time$utc, targetPosix);
		var time = A2(
			$elm$core$String$join,
			':',
			A2(
				$elm$core$List$map,
				prefixZero,
				_List_fromArray(
					[hour, min, sec])));
		var day = A2($elm$time$Time$toDay, $elm$time$Time$utc, targetPosix);
		var date = A2(
			$elm$core$String$join,
			'/',
			A2(
				$elm$core$List$map,
				prefixZero,
				_List_fromArray(
					[year, month, day])));
		var whole = A2(
			$elm$core$String$join,
			' - ',
			_List_fromArray(
				[date, time]));
		return whole;
	});
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
var $author$project$Utils$Markdown$viewSpacing = A2($mdgriffith$elm_ui$Element$spacingXY, 0, 15);
var $author$project$Common$Colors$yellow = A3($mdgriffith$elm_ui$Element$rgb255, 220, 220, 20);
var $author$project$Views$Chat$msgView = function (msg) {
	var paddedTimeText = A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$paddingEach(
				{cQ: 0, dK: 10, eh: 0, e5: 0})
			]),
		A2(
			$author$project$Common$Contents$timeText,
			$author$project$Utils$Utils$posixSecToPosix(msg.bY),
			$author$project$Utils$Utils$posixSecToPosix(msg.bY)));
	var msgFromClient = msg.bN;
	var _v0 = msgFromClient.bO;
	switch (_v0) {
		case 1:
			return A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$green)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text(
						$author$project$Views$Chat$capUsername(msg.aQ) + ' joined.'),
						paddedTimeText
					]));
		case 2:
			return A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$yellow)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text(
						$elm_community$string_extra$String$Extra$quote(
							$author$project$Views$Chat$capUsername(msg.aQ)) + (' changed their name to ' + ($elm_community$string_extra$String$Extra$quote(
							$author$project$Views$Chat$capUsername(
								$joneshf$elm_tagged$Tagged$untag(msgFromClient.dT))) + '.'))),
						paddedTimeText
					]));
		case 4:
			return A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$red)
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text(
						$author$project$Views$Chat$capUsername(msg.aQ) + ' left.'),
						paddedTimeText
					]));
		case 0:
			return A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[$author$project$Utils$Markdown$viewSpacing]),
				$author$project$Utils$Markdown$render(
					$joneshf$elm_tagged$Tagged$untag(msgFromClient.dT)));
		default:
			return $mdgriffith$elm_ui$Element$none;
	}
};
var $author$project$Views$Chat$msgBundleView = function (bundle) {
	return A2(
		$mdgriffith$elm_ui$Element$textColumn,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 15)
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
									$mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[$mdgriffith$elm_ui$Element$Font$bold]),
									$mdgriffith$elm_ui$Element$text(
										$author$project$Views$Chat$capUsername(bundle.aQ))),
									A2($author$project$Common$Contents$timeText, bundle.ce, bundle.ce)
								]));
					} else {
						return $mdgriffith$elm_ui$Element$none;
					}
				}()
				]),
			A2($elm$core$List$map, $author$project$Views$Chat$msgView, bundle.L)));
};
var $mdgriffith$elm_ui$Element$Input$multiline = F2(
	function (attrs, multi) {
		return A3(
			$mdgriffith$elm_ui$Element$Input$textHelper,
			{y: $elm$core$Maybe$Nothing, C: multi.eA, j: $mdgriffith$elm_ui$Element$Input$TextArea},
			attrs,
			{a0: multi.a0, dZ: multi.dZ, ea: multi.ea, eR: multi.eR});
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
			if (dict.$ === -2) {
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
var $elm$html$Html$source = _VirtualDom_node('source');
var $author$project$Views$Chat$userView = function (_v0) {
	var userId = _v0.a;
	var username = _v0.b;
	return $author$project$Common$Contents$plainPara(
		A2($author$project$Utils$Utils$capString, 16, username) + (' (' + ($elm$core$String$fromInt(userId) + ')')));
};
var $mdgriffith$elm_ui$Internal$Model$Padding = F5(
	function (a, b, c, d, e) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Model$Spaced = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$extractSpacingAndPadding = function (attrs) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (attr, _v0) {
				var pad = _v0.a;
				var spacing = _v0.b;
				return _Utils_Tuple2(
					function () {
						if (!pad.$) {
							var x = pad.a;
							return pad;
						} else {
							if ((attr.$ === 4) && (attr.b.$ === 7)) {
								var _v3 = attr.b;
								var name = _v3.a;
								var t = _v3.b;
								var r = _v3.c;
								var b = _v3.d;
								var l = _v3.e;
								return $elm$core$Maybe$Just(
									A5($mdgriffith$elm_ui$Internal$Model$Padding, name, t, r, b, l));
							} else {
								return $elm$core$Maybe$Nothing;
							}
						}
					}(),
					function () {
						if (!spacing.$) {
							var x = spacing.a;
							return spacing;
						} else {
							if ((attr.$ === 4) && (attr.b.$ === 5)) {
								var _v6 = attr.b;
								var name = _v6.a;
								var x = _v6.b;
								var y = _v6.c;
								return $elm$core$Maybe$Just(
									A3($mdgriffith$elm_ui$Internal$Model$Spaced, name, x, y));
							} else {
								return $elm$core$Maybe$Nothing;
							}
						}
					}());
			}),
		_Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
		attrs);
};
var $mdgriffith$elm_ui$Element$wrappedRow = F2(
	function (attrs, children) {
		var _v0 = $mdgriffith$elm_ui$Internal$Model$extractSpacingAndPadding(attrs);
		var padded = _v0.a;
		var spaced = _v0.b;
		if (spaced.$ === 1) {
			return A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asRow,
				$mdgriffith$elm_ui$Internal$Model$div,
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aj + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.J + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bj)))),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
							attrs))),
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
		} else {
			var _v2 = spaced.a;
			var spaceName = _v2.a;
			var x = _v2.b;
			var y = _v2.c;
			var newPadding = function () {
				if (!padded.$) {
					var _v5 = padded.a;
					var name = _v5.a;
					var t = _v5.b;
					var r = _v5.c;
					var b = _v5.d;
					var l = _v5.e;
					if ((_Utils_cmp(r, x / 2) > -1) && (_Utils_cmp(b, y / 2) > -1)) {
						var newTop = t - (y / 2);
						var newRight = r - (x / 2);
						var newLeft = l - (x / 2);
						var newBottom = b - (y / 2);
						return $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_ui$Internal$Model$StyleClass,
								$mdgriffith$elm_ui$Internal$Flag$padding,
								A5(
									$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
									A4($mdgriffith$elm_ui$Internal$Model$paddingNameFloat, newTop, newRight, newBottom, newLeft),
									newTop,
									newRight,
									newBottom,
									newLeft)));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}();
			if (!newPadding.$) {
				var pad = newPadding.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asRow,
					$mdgriffith$elm_ui$Internal$Model$div,
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aj + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.J + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bj)))),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
								_Utils_ap(
									attrs,
									_List_fromArray(
										[pad]))))),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
			} else {
				var halfY = -(y / 2);
				var halfX = -(x / 2);
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					attrs,
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								$mdgriffith$elm_ui$Internal$Model$element,
								$mdgriffith$elm_ui$Internal$Model$asRow,
								$mdgriffith$elm_ui$Internal$Model$div,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aj + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.J + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bj)))),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Internal$Model$Attr(
											A2(
												$elm$html$Html$Attributes$style,
												'margin',
												$elm$core$String$fromFloat(halfY) + ('px' + (' ' + ($elm$core$String$fromFloat(halfX) + 'px'))))),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Internal$Model$Attr(
												A2(
													$elm$html$Html$Attributes$style,
													'width',
													'calc(100% + ' + ($elm$core$String$fromInt(x) + 'px)'))),
											A2(
												$elm$core$List$cons,
												$mdgriffith$elm_ui$Internal$Model$Attr(
													A2(
														$elm$html$Html$Attributes$style,
														'height',
														'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))),
												A2(
													$elm$core$List$cons,
													A2(
														$mdgriffith$elm_ui$Internal$Model$StyleClass,
														$mdgriffith$elm_ui$Internal$Flag$spacing,
														A3($mdgriffith$elm_ui$Internal$Model$SpacingStyle, spaceName, x, y)),
													_List_Nil))))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(children))
							])));
			}
		}
	});
var $author$project$Views$Chat$chatView = F2(
	function (model, viewportWidth) {
		return A2(
			$mdgriffith$elm_ui$Element$row,
			_Utils_ap(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
						A2($mdgriffith$elm_ui$Element$spacingXY, $author$project$Views$Chat$sideColumnGap, 0)
					]),
				function () {
					var _v0 = model.dW;
					if (!_v0.$) {
						return _List_Nil;
					} else {
						var newNameInput = _v0.a;
						return _List_fromArray(
							[
								function () {
								var bgColor = $mdgriffith$elm_ui$Element$Background$color(
									A3($mdgriffith$elm_ui$Element$rgb255, 69, 102, 122));
								return $mdgriffith$elm_ui$Element$inFront(
									A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
											]),
										A2(
											$mdgriffith$elm_ui$Element$column,
											_List_fromArray(
												[
													$mdgriffith$elm_ui$Element$centerX,
													$mdgriffith$elm_ui$Element$centerY,
													A2($mdgriffith$elm_ui$Element$paddingXY, 35, 30),
													A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20),
													bgColor
												]),
											_List_fromArray(
												[
													A2(
													$mdgriffith$elm_ui$Element$el,
													_List_fromArray(
														[$mdgriffith$elm_ui$Element$centerX]),
													$mdgriffith$elm_ui$Element$text('Changing name to')),
													A2(
													$mdgriffith$elm_ui$Element$Input$text,
													_List_fromArray(
														[bgColor]),
													{
														a0: $mdgriffith$elm_ui$Element$Input$labelHidden(''),
														dZ: $author$project$Chat$OnNewNameInput,
														ea: $elm$core$Maybe$Nothing,
														eR: newNameInput
													}),
													A2(
													$mdgriffith$elm_ui$Element$row,
													_List_fromArray(
														[
															$mdgriffith$elm_ui$Element$centerX,
															A2($mdgriffith$elm_ui$Element$spacingXY, 40, 0)
														]),
													function () {
														var style = A2(
															$elm$core$List$cons,
															bgColor,
															$author$project$Common$Styles$buttonStyle(5));
														return _List_fromArray(
															[
																A2(
																$mdgriffith$elm_ui$Element$Input$button,
																style,
																{
																	a0: $mdgriffith$elm_ui$Element$text('Confirm'),
																	d$: $elm$core$Maybe$Just(
																		$author$project$Chat$OnFinishNameChange(true))
																}),
																A2(
																$mdgriffith$elm_ui$Element$Input$button,
																style,
																{
																	a0: $mdgriffith$elm_ui$Element$text('Cancel'),
																	d$: $elm$core$Maybe$Just(
																		$author$project$Chat$OnFinishNameChange(false))
																})
															]);
													}())
												]))));
							}()
							]);
					}
				}()),
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$html(
					A2(
						$elm$html$Html$audio,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('notificationAudio')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$source,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('/static/sounds/notification.wav'),
										$elm$html$Html$Attributes$type_('audio/wav')
									]),
								_List_Nil)
							]))),
					A2(
					$mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$width(
							A2(
								$mdgriffith$elm_ui$Element$maximum,
								$author$project$Views$Chat$chatColumnMaxWidthPx(viewportWidth),
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
									{cQ: 0, dK: 0, eh: 30, e5: 0}),
									A2($mdgriffith$elm_ui$Element$spacingXY, 0, 30),
									$mdgriffith$elm_ui$Element$scrollbarY,
									$mdgriffith$elm_ui$Element$htmlAttribute(
									$elm$html$Html$Attributes$id($author$project$Views$Chat$msgsViewHtmlId)),
									$mdgriffith$elm_ui$Element$htmlAttribute(
									A2(
										$elm$html$Html$Events$on,
										'scroll',
										$elm$json$Json$Decode$succeed(
											$author$project$Chat$OnMsgsViewEvent($author$project$Chat$OnManualScrolled))))
								]),
							A2(
								$elm$core$List$map,
								$author$project$Views$Chat$msgBundleView,
								$author$project$Chat$mkMsgBundles(model.L))),
							A2(
							$mdgriffith$elm_ui$Element$el,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$height(
									$mdgriffith$elm_ui$Element$px(20)),
									A2($mdgriffith$elm_ui$Element$paddingXY, 0, 10),
									$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$grey)
								]),
							function () {
								if (!$elm$core$List$length(model.e9)) {
									return $mdgriffith$elm_ui$Element$none;
								} else {
									var typingUsersNames = A2(
										$elm$core$List$map,
										function (userId) {
											return $author$project$Views$Chat$capUsername(
												A2(
													$elm$core$Maybe$withDefault,
													'[ErrorUsername]',
													A2($elm$core$Dict$get, userId, model.ck)));
										},
										model.e9);
									return $mdgriffith$elm_ui$Element$text(
										A2($elm$core$String$join, ', ', typingUsersNames) + ((($elm$core$List$length(model.e9) === 1) ? ' is ' : ' are ') + 'typing...'));
								}
							}()),
							A2(
							$mdgriffith$elm_ui$Element$el,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$height(
									$mdgriffith$elm_ui$Element$px(40)),
									$mdgriffith$elm_ui$Element$centerX
								]),
							function () {
								var _v1 = model.et;
								if (!_v1) {
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
											a0: $mdgriffith$elm_ui$Element$text('New Messages Received'),
											d$: $elm$core$Maybe$Just(
												$author$project$Chat$OnMsgsViewEvent($author$project$Chat$OnNewMsgHintClicked))
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
											$author$project$Chat$OnChatInputFocal(true))),
										$mdgriffith$elm_ui$Element$htmlAttribute(
										$elm$html$Html$Events$onBlur(
											$author$project$Chat$OnChatInputFocal(false)))
									]),
								{
									a0: A2($mdgriffith$elm_ui$Element$Input$labelAbove, _List_Nil, $mdgriffith$elm_ui$Element$none),
									dZ: $author$project$Chat$MessageInput,
									ea: $elm$core$Maybe$Nothing,
									eA: false,
									eR: $joneshf$elm_tagged$Tagged$untag(model.dw)
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
										a0: $mdgriffith$elm_ui$Element$text('Send'),
										d$: $elm$core$Maybe$Just($author$project$Chat$MessageSend)
									}),
									A2(
									$mdgriffith$elm_ui$Element$Input$button,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$padding(5),
											$mdgriffith$elm_ui$Element$Background$color(
											A3($mdgriffith$elm_ui$Element$rgb255, 80, 80, 80))
										]),
									{
										a0: $mdgriffith$elm_ui$Element$text('Change Name'),
										d$: $elm$core$Maybe$Just($author$project$Chat$OnBeginNameChange)
									}),
									function () {
									var mkEmoji = function (hex) {
										return A2(
											$mdgriffith$elm_ui$Element$Input$button,
											_List_fromArray(
												[
													$mdgriffith$elm_ui$Element$width(
													$mdgriffith$elm_ui$Element$px(48)),
													$mdgriffith$elm_ui$Element$height(
													$mdgriffith$elm_ui$Element$px(48)),
													$mdgriffith$elm_ui$Element$Background$image(
													$author$project$Emoji$hexToPath(hex))
												]),
											{
												a0: $mdgriffith$elm_ui$Element$none,
												d$: $elm$core$Maybe$Just(
													$author$project$Chat$OnEmojiChosen(hex))
											});
									};
									var emojiPicker = A2(
										$mdgriffith$elm_ui$Element$wrappedRow,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width(
												$mdgriffith$elm_ui$Element$px(370)),
												$mdgriffith$elm_ui$Element$height(
												$mdgriffith$elm_ui$Element$px(400)),
												$mdgriffith$elm_ui$Element$scrollbarY,
												$mdgriffith$elm_ui$Element$padding(5),
												$mdgriffith$elm_ui$Element$Border$width(2),
												$mdgriffith$elm_ui$Element$Border$rounded(4),
												$mdgriffith$elm_ui$Element$htmlAttribute(
												$elm$html$Html$Attributes$id($author$project$Views$Chat$emojiPickerHtmlId)),
												$mdgriffith$elm_ui$Element$htmlAttribute(
												A2(
													$elm$html$Html$Events$on,
													'scroll',
													$elm$json$Json$Decode$succeed($author$project$Chat$OnEmojiScrolled)))
											]),
										A2($elm$core$List$map, mkEmoji, model.dc));
									return A2(
										$mdgriffith$elm_ui$Element$Input$button,
										_Utils_ap(
											$author$project$Common$Styles$buttonStyle(5),
											model.dD ? _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$above(
													A2(
														$mdgriffith$elm_ui$Element$el,
														_List_fromArray(
															[
																$mdgriffith$elm_ui$Element$paddingEach(
																{cQ: 10, dK: 0, eh: 0, e5: 0})
															]),
														emojiPicker))
												]) : _List_Nil),
										{
											a0: $mdgriffith$elm_ui$Element$text('Emoji'),
											d$: $elm$core$Maybe$Just($author$project$Chat$OnEmojiToggled)
										});
								}()
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
							{cQ: 0, dK: 2, eh: 0, e5: 0})
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
								$elm$core$Dict$toList(model.ck))),
							A2(
							$mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$height(
									$mdgriffith$elm_ui$Element$fillPortion(1)),
									$mdgriffith$elm_ui$Element$paddingEach(
									{cQ: 0, dK: 0, eh: 0, e5: 20}),
									A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10),
									$mdgriffith$elm_ui$Element$Border$widthEach(
									{cQ: 0, dK: 0, eh: 0, e5: 2})
								]),
							_List_fromArray(
								[
									$author$project$Common$Contents$plainPara(
									'Current Users: ' + $elm$core$String$fromInt(
										$elm$core$Dict$size(model.ck))),
									$author$project$Common$Contents$plainPara(
									'Join Count: ' + $elm$core$String$fromInt(model.dI)),
									$author$project$Common$Contents$plainPara(
									'Max Join Count: ' + function () {
										var _v2 = model.bL;
										if (_v2.$ === 1) {
											return 'Unlimited';
										} else {
											var n = _v2.a;
											return $elm$core$String$fromInt(n);
										}
									}())
								]))
						]))
				]));
	});
var $author$project$Common$Styles$widthConstraint = $mdgriffith$elm_ui$Element$width(
	A2($mdgriffith$elm_ui$Element$maximum, 750, $mdgriffith$elm_ui$Element$fill));
var $author$project$Common$Contents$footer = A2(
	$mdgriffith$elm_ui$Element$row,
	_List_fromArray(
		[
			$author$project$Common$Styles$widthConstraint,
			A2($mdgriffith$elm_ui$Element$paddingXY, 0, 20),
			$mdgriffith$elm_ui$Element$Border$widthEach(
			{cQ: 0, dK: 0, eh: 0, e5: 2})
		]),
	_List_fromArray(
		[
			A2($author$project$Common$Contents$link, $author$project$Common$Urls$rootUrl, 'Hideout Home')
		]));
var $author$project$Views$Chat$mkErrView = function (content) {
	return A2(
		$mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width(
				A2($mdgriffith$elm_ui$Element$maximum, 750, $mdgriffith$elm_ui$Element$fill)),
				A2($mdgriffith$elm_ui$Element$spacingXY, 0, 100)
			]),
		_List_fromArray(
			[content, $author$project$Common$Contents$footer]));
};
var $author$project$Views$Chat$view = F2(
	function (status, viewportWidth) {
		switch (status.$) {
			case 0:
				var model = status.a;
				return A2($author$project$Views$Chat$chatView, model, viewportWidth);
			case 1:
				var chatId = status.a;
				return $author$project$Views$Chat$mkErrView(
					$author$project$Common$Contents$plainPara('Waiting for websocket to be opened...'));
			case 2:
				return $author$project$Views$Chat$mkErrView(
					$author$project$Common$Contents$plainPara('\n                    Error when opening websocket. Maybe server is unreachable?\n                    '));
			case 3:
				var err = status.a;
				var errContent = function () {
					if (!err) {
						return A2(
							$mdgriffith$elm_ui$Element$paragraph,
							_List_Nil,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$text('\n                                Hi, welcome to Hideout! Unfortunately, this chat room has reached the maximum number of times it can be joined. Read more about what this implies \n                                '),
									A2(
									$author$project$Common$Contents$newTabLink,
									$author$project$Views$About$sectionToUrl(5),
									'here'),
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
				return $author$project$Views$Chat$mkErrView(errContent);
			case 4:
				var idStr = status.a;
				return $author$project$Common$Contents$plainPara('Can\'t find DOM element with ID ' + idStr);
			default:
				return $author$project$Views$Chat$mkErrView(
					$author$project$Common$Contents$plainPara('Chat status is NotChatting. Internal logical error?'));
		}
	});
var $author$project$CoreTypes$DispChatMaxJoinCountInput = function (a) {
	return {$: 12, a: a};
};
var $author$project$CoreTypes$PersistChatMaxJoinCountInput = function (a) {
	return {$: 15, a: a};
};
var $author$project$CoreTypes$SpawnDispChat = {$: 13};
var $author$project$CoreTypes$SpawnPersistChat = {$: 16};
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
				a0: $mdgriffith$elm_ui$Element$text(labelStr),
				d$: $elm$core$Maybe$Just(msg)
			});
	});
var $author$project$Common$Urls$frontendReadLetterUrl = A2(
	$elm$url$Url$Builder$absolute,
	_List_fromArray(
		['read-letter']),
	_List_Nil);
var $author$project$Common$Styles$inlineInputStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$width(
		$mdgriffith$elm_ui$Element$px(100)),
		$mdgriffith$elm_ui$Element$height(
		A2($mdgriffith$elm_ui$Element$maximum, 40, $mdgriffith$elm_ui$Element$fill)),
		$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor)
	]);
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
					$author$project$Common$Styles$inlineInputStyle,
					{
						a0: $mdgriffith$elm_ui$Element$Input$labelHidden(''),
						dZ: msg,
						ea: $elm$core$Maybe$Nothing,
						eR: input
					})
				]));
	});
var $author$project$Common$Contents$posIntInputHint = function (input) {
	var _v0 = $author$project$Utils$Types$strToPosIntInput(input);
	if (!_v0.$) {
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
				A2($author$project$Views$ConfigChat$numParticipantsInput, $author$project$CoreTypes$DispChatMaxJoinCountInput, model.c9),
				$author$project$Common$Contents$posIntInputHint(model.c9),
				A2(
				$mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$paddingEach(
						{cQ: 0, dK: 0, eh: 0, e5: 10})
					]),
				A2($author$project$Common$Contents$borderedButton, $author$project$CoreTypes$SpawnDispChat, 'Start a disposable chat!')),
				function () {
				var padding = $mdgriffith$elm_ui$Element$paddingEach(
					{cQ: 0, dK: 0, eh: 0, e5: 20});
				var _v0 = model.ey;
				switch (_v0.$) {
					case 0:
						return $mdgriffith$elm_ui$Element$none;
					case 1:
						return A2(
							$mdgriffith$elm_ui$Element$paragraph,
							_List_fromArray(
								[padding]),
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$text('Waiting for response from the server...')
								]));
					case 2:
						var err = _v0.a;
						return A2(
							$mdgriffith$elm_ui$Element$paragraph,
							_List_fromArray(
								[
									padding,
									$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$red)
								]),
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$text('Error reaching server!')
								]));
					default:
						return $mdgriffith$elm_ui$Element$none;
				}
			}(),
				A2(
				$mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$paddingEach(
						{cQ: 0, dK: 0, eh: 0, e5: 200})
					]),
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$text('A '),
						$author$project$Common$Contents$italicText('persistent'),
						$mdgriffith$elm_ui$Element$text('\n                 chat is more convenient. You can bookmark the chat, and send private messages to your contacts without having to make a new chat every time. This especially helps if you need to circumvent censorship. Sharing too many Hideout links may draw unwanted attention.\n                ')
					])),
				A2($author$project$Views$ConfigChat$numParticipantsInput, $author$project$CoreTypes$PersistChatMaxJoinCountInput, model.d9),
				$author$project$Common$Contents$posIntInputHint(model.d9),
				A2(
				$mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$paddingEach(
						{cQ: 0, dK: 0, eh: 0, e5: 10})
					]),
				A2($author$project$Common$Contents$borderedButton, $author$project$CoreTypes$SpawnPersistChat, 'Start a persistent chat!')),
				function () {
				var padding = $mdgriffith$elm_ui$Element$paddingEach(
					{cQ: 0, dK: 0, eh: 0, e5: 20});
				var _v1 = model.ez;
				switch (_v1.$) {
					case 0:
						return $mdgriffith$elm_ui$Element$none;
					case 1:
						return A2(
							$mdgriffith$elm_ui$Element$paragraph,
							_List_fromArray(
								[padding]),
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$text('Waiting for response from the server...')
								]));
					case 2:
						var error = _v1.a;
						return A2(
							$mdgriffith$elm_ui$Element$paragraph,
							_List_fromArray(
								[
									padding,
									$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$red)
								]),
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$text('Error reaching server!')
								]));
					default:
						var letterId = _v1.a;
						return A2(
							$mdgriffith$elm_ui$Element$textColumn,
							_List_fromArray(
								[
									padding,
									A2($mdgriffith$elm_ui$Element$spacingXY, 0, 20)
								]),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Element$paragraph,
									_List_Nil,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$text('\n                            A disposable letter that contains the instruction to join the chat has been generated. Share the \n                            '),
											$author$project$Common$Contents$underlinedText('link'),
											$mdgriffith$elm_ui$Element$text('\n                             (not the content) to the letter below with your contacts.\n                            ')
										])),
									$author$project$Common$Contents$plainPara(
									model.d5 + ($author$project$Common$Urls$frontendReadLetterUrl + ('/' + $elm_community$string_extra$String$Extra$unquote(letterId))))
								]));
				}
			}()
			]));
};
var $author$project$Utils$Errors$httpErrToStr = function (err) {
	switch (err.$) {
		case 0:
			var str = err.a;
			return 'Bad URL ' + str;
		case 1:
			return 'Timeout';
		case 2:
			return '\n            Can\'t reach server. Either your network has a problem, or server is down.\n            ';
		case 3:
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
	var windowWidth = model.fe.fe.at;
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
						{cQ: 0, dK: 0, eh: 0, e5: 20})
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
				var _v0 = model.dO.ed;
				switch (_v0.$) {
					case 1:
						return $author$project$Common$Contents$plainPara('Waiting for the letter from server..');
					case 2:
						var result = _v0.a;
						if (result.$ === 1) {
							var err = result.a;
							if ((err.$ === 3) && (err.a === 404)) {
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
													{cQ: 100, dK: 0, eh: 0, e5: 20}),
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
															A2(
															$author$project$Common$Contents$newTabLink,
															$author$project$Views$About$sectionToUrl(5),
															'here'),
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
														$author$project$Utils$Utils$intToOrdStr(letterMeta.ee)),
														$mdgriffith$elm_ui$Element$text(' time.')
													])),
												A2(
												$mdgriffith$elm_ui$Element$paragraph,
												_List_Nil,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('It can be read at most '),
														$mdgriffith$elm_ui$Element$text(
														$elm$core$String$fromInt(letterMeta.dL.am)),
														$mdgriffith$elm_ui$Element$text(' times.')
													]))
											])),
										A2(
										$mdgriffith$elm_ui$Element$paragraph,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$paddingEach(
												{cQ: 20, dK: 0, eh: 0, e5: 0})
											]),
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$text('Below is the letter.')
											])),
										A2(
										$mdgriffith$elm_ui$Element$column,
										_List_fromArray(
											[
												$author$project$Utils$Markdown$viewSpacing,
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$paddingEach(
												{cQ: 0, dK: 0, eh: 0, e5: 60}),
												$mdgriffith$elm_ui$Element$Border$widthEach(
												{cQ: 0, dK: 0, eh: 0, e5: 2})
											]),
										$author$project$Utils$Markdown$render(letterMeta.dL.aa))
									]));
						}
					default:
						return $author$project$Common$Contents$plainPara('Error: Unaddressed UserStatus case!');
				}
			}()
			]));
};
var $author$project$CoreTypes$LetterInput = function (a) {
	return {$: 7, a: a};
};
var $author$project$CoreTypes$LetterMaxReadCountInput = function (a) {
	return {$: 8, a: a};
};
var $author$project$CoreTypes$LetterPersistInput = function (a) {
	return {$: 9, a: a};
};
var $author$project$CoreTypes$LetterSend = {$: 10};
var $mdgriffith$elm_ui$Internal$Model$Left = 0;
var $mdgriffith$elm_ui$Element$alignLeft = $mdgriffith$elm_ui$Internal$Model$AlignX(0);
var $mdgriffith$elm_ui$Element$Input$tabindex = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$tabindex);
var $mdgriffith$elm_ui$Element$Input$checkbox = F2(
	function (attrs, _v0) {
		var label = _v0.a0;
		var icon = _v0.dp;
		var checked = _v0.cY;
		var onChange = _v0.dZ;
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
var $mdgriffith$elm_ui$Element$Input$OnRight = 0;
var $mdgriffith$elm_ui$Element$Input$labelRight = $mdgriffith$elm_ui$Element$Input$Label(0);
var $author$project$Common$Styles$lineSpacing = A2($mdgriffith$elm_ui$Element$spacingXY, 0, 10);
var $author$project$Views$WriteLetter$view = function (model) {
	var windowWidth = model.fe.fe.at;
	var persistInput = A2(
		$mdgriffith$elm_ui$Element$Input$checkbox,
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Element$paddingXY, 0, 20)
			]),
		{
			cY: model.dM,
			dp: $mdgriffith$elm_ui$Element$Input$defaultCheckbox,
			a0: A2(
				$mdgriffith$elm_ui$Element$Input$labelRight,
				_List_Nil,
				$mdgriffith$elm_ui$Element$text('Disk Persistence')),
			dZ: $author$project$CoreTypes$LetterPersistInput
		});
	var maxReadCountInput = A2(
		$mdgriffith$elm_ui$Element$row,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$paddingEach(
				{cQ: 10, dK: 0, eh: 0, e5: 40}),
				A2($mdgriffith$elm_ui$Element$spacingXY, 10, 0)
			]),
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$text('This letter can be read '),
				A2(
				$mdgriffith$elm_ui$Element$Input$text,
				$author$project$Common$Styles$inlineInputStyle,
				{
					a0: $mdgriffith$elm_ui$Element$Input$labelHidden(''),
					dZ: $author$project$CoreTypes$LetterMaxReadCountInput,
					ea: $elm$core$Maybe$Nothing,
					eR: model.dN.am
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
			[letterInputWidthConstraint, $author$project$Utils$Markdown$viewSpacing, $mdgriffith$elm_ui$Element$alignTop]),
		$author$project$Utils$Markdown$render(model.dN.aa));
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
			a0: A2($mdgriffith$elm_ui$Element$Input$labelAbove, _List_Nil, $mdgriffith$elm_ui$Element$none),
			dZ: $author$project$CoreTypes$LetterInput,
			ea: $elm$core$Maybe$Nothing,
			eA: false,
			eR: model.dN.aa
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
						$author$project$Common$Contents$posIntInputHint(model.dN.am),
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
						var _v0 = model.dO.fh;
						switch (_v0.$) {
							case 0:
								return letterInputBox;
							case 1:
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
								if (result.$ === 1) {
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
														$elm$core$String$fromInt(info.am)),
														$mdgriffith$elm_ui$Element$text(' times, at:')
													])),
												$author$project$Common$Contents$plainPara(
												model.d5 + ($author$project$Common$Urls$frontendReadLetterUrl + ('/' + $elm_community$string_extra$String$Extra$unquote(info.dq))))
											]));
								}
						}
					}(),
						$author$project$Views$WriteLetter$divider,
						preview
					])),
				function () {
				var _v2 = model.dO.fh;
				if (!_v2.$) {
					return A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$paddingEach(
								{cQ: 0, dK: 0, eh: 0, e5: 20})
							]),
						A2(
							$mdgriffith$elm_ui$Element$Input$button,
							$author$project$Common$Styles$buttonStyle(5),
							{
								a0: $mdgriffith$elm_ui$Element$text('Send'),
								d$: $elm$core$Maybe$Just($author$project$CoreTypes$LetterSend)
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
var $author$project$Main$viewModel = function (model) {
	return {
		aa: _List_fromArray(
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
							$author$project$Common$Styles$windowPadding(model.fe.fe.at)
						]),
					function () {
						var _v0 = model.ej;
						switch (_v0.$) {
							case 0:
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
														a0: $mdgriffith$elm_ui$Element$text('> Write a letter.'),
														cj: $author$project$Common$Urls$frontendWriteLetterUrl
													}),
													A2(
													$mdgriffith$elm_ui$Element$link,
													_List_Nil,
													{
														a0: $mdgriffith$elm_ui$Element$text('> Start a chat.'),
														cj: $author$project$Common$Urls$configChatUrl
													}),
													A2(
													$mdgriffith$elm_ui$Element$row,
													_List_fromArray(
														[
															A2($mdgriffith$elm_ui$Element$paddingXY, 0, 10)
														]),
													_List_fromArray(
														[
															$mdgriffith$elm_ui$Element$text('> Join a chat: '),
															A2(
															$mdgriffith$elm_ui$Element$Input$text,
															_List_fromArray(
																[
																	$mdgriffith$elm_ui$Element$width(
																	$mdgriffith$elm_ui$Element$px(400)),
																	$mdgriffith$elm_ui$Element$height(
																	A2($mdgriffith$elm_ui$Element$maximum, 40, $mdgriffith$elm_ui$Element$fill)),
																	$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor)
																]),
															{
																a0: $mdgriffith$elm_ui$Element$Input$labelHidden(''),
																dZ: $author$project$CoreTypes$JoinChatInput,
																ea: $elm$core$Maybe$Just(
																	A2(
																		$mdgriffith$elm_ui$Element$Input$placeholder,
																		_List_Nil,
																		$mdgriffith$elm_ui$Element$text('Room ID'))),
																eR: model.dH
															}),
															A2(
															$mdgriffith$elm_ui$Element$el,
															_List_fromArray(
																[
																	A2($mdgriffith$elm_ui$Element$paddingXY, 10, 0)
																]),
															A2(
																$mdgriffith$elm_ui$Element$Input$button,
																_List_fromArray(
																	[
																		$mdgriffith$elm_ui$Element$padding(5),
																		$mdgriffith$elm_ui$Element$Border$width(2),
																		$mdgriffith$elm_ui$Element$Border$rounded(6)
																	]),
																{
																	a0: $mdgriffith$elm_ui$Element$text('Join'),
																	d$: $elm$core$Maybe$Just($author$project$CoreTypes$JoinChat)
																}))
														])),
													A2(
													$mdgriffith$elm_ui$Element$link,
													_List_Nil,
													{
														a0: $mdgriffith$elm_ui$Element$text('> How does it work?'),
														cj: $author$project$Common$Urls$aboutUrl
													})
												]))
										]));
							case 1:
								var sectionFromRoute = _v0.a;
								var aboutPageView = A2($author$project$Views$About$view, model.fe.fe.at, model.cq);
								return A2($mdgriffith$elm_ui$Element$map, $author$project$CoreTypes$AboutPageMsg, aboutPageView);
							case 2:
								var id = _v0.a;
								return $author$project$Views$ReadLetter$view(model);
							case 3:
								return $author$project$Views$WriteLetter$view(model);
							case 4:
								var chatId = _v0.a;
								return A2(
									$mdgriffith$elm_ui$Element$map,
									$author$project$CoreTypes$ChatElmMsg,
									A2($author$project$Views$Chat$view, model.cX, model.fe.fe.at));
							case 5:
								return $author$project$Views$ConfigChat$view(model);
							default:
								return $mdgriffith$elm_ui$Element$text('404');
						}
					}()))
			]),
		cf: 'Hideout'
	};
};
var $author$project$Main$view = function (state) {
	if (state.$ === 1) {
		var model = state.a;
		return $author$project$Main$viewModel(model);
	} else {
		return {
			aa: _List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$layout,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Background$color($author$project$Common$Colors$bgColor),
							$mdgriffith$elm_ui$Element$Font$color($author$project$Common$Colors$white)
						]),
					$mdgriffith$elm_ui$Element$text('Error state!'))
				]),
			cf: 'Hideout got an error D:'
		};
	}
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{dv: $author$project$Main$init, d1: $author$project$CoreTypes$UrlChanged, d2: $author$project$CoreTypes$UrlRequested, eJ: $author$project$Main$subscriptions, fc: $author$project$Main$update, fd: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));