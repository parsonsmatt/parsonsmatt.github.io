---
title: "Postfix Arithmetic in Javascript"
date: 2014-07-07 12:00:00
layout: post
category: programming
---

## Getting Started

I learned about the [stack](http://en.wikipedia.org/wiki/Stack_\(abstract_data_type\)) data  type in class, and the specific given example was a postfix arithmetic algorithm. I've also been learning Javascript from [Eloquent Javascript](http://eloquentjavascript.net/) and [Javascript: The Good Parts](http://www.amazon.com/JavaScript-Good-Parts-Douglas-Crockford/dp/0596517742). It dawned on me that this was a great way to experiment with Javascript's functional programming elements, get some experience implementing a basic data structure, and documenting the writing process. So I decided to get cracking!

## Postfix Arithmetic

Postfix arithmetic means that the operator goes after the two number. Formally, you have `Operand1 Operand2 Operator`. It is contrasted with infix arithmetic, where you use the operator in between the operands. A simple example contrasts `5 + 3` with its equivalent `5 3 +`. A postfix expression can be an operand in another postfix expression: `5 3 + 3 *` is equivalent to `(5 + 3 ) * 3`. Since an expression can serve as an operand, it's unnecessary to use parentheses to specify operation order. 

The infix expression `(3 * 4 / (2 + 5)) * (3 + 4)` is equivalent to the postfix expression `3 4 * 2 5 + / 3 4 + *`. This seems a bit cryptic. I'll go through the expression piece by piece and solve it. `3 4 *` is the first expression and it evaluates to 12. `2 5 +` is the second expression and evaluates to 7. The original expression can be thought of as `(3 4 *) (2 5 +) /` which makes it a bit easier to see how the operator takes the two prior expressions as operands. The rest is easy enough to evaluate with that. 

## The Algorithm

You can easily use a stack to parse postfix expressions. The general algorithm looks like this:

{% highlight javascript linenos %}
for each (element in expression) {
    if ( element is an operator) {
	    push (element(pop,pop);
    }
	else {
        push next;
    }

}
{% endhighlight %}

The algorithm goes through each element and checks to see if it's an operator or operand. If it's an operand, it pushes it onto the stack. If it's an operator, it takes the previous two values on the stack and pushes the computation back to the stack. 

## The Javascript

This seems like an obvious use of functional programming! Javascript already implements the stack operations `push` and `pop` in the basic array methods, so we can use a simple array for this. First, we'll consider a Properly Formatted string of characters (presumably acquired from some other function which validates user input) which we'll split into an array.

{% highlight javascript linenos %}
var expression = "3 4 * 2 5 + / 3 4 + *";
var postfix = expression.split(" ");
var postfixStack = [];
{% endhighlight %}

Then we'll go through the array and either push or compute:

{% highlight javascript linenos %}
postfix.forEach( function(current) {
    if ( isOperator(current) ) {
        postfixStack.push( 
            compute( postfixStack.pop(), 
				symbolToOperator(current), 
				postfixStack.pop() 
			)
        );
    }
    else {
        postfixStack.push(current);
    }   
});
{% endhighlight %}

This makes good enough sense. Let's define those functions:

{% highlight javascript linenos %}
function isOperator(toCheck) {
    switch (toCheck) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
            return true;
        default:
            return false;
    }
}

function compute(a, operator, b) {
    return operator(a,b); 
}

function symbolToOperator(symbol) {
    switch (symbol) {
        case '+': return plus;
        case '-': return minus;
        case '*': return multiply;
        case '/': return divide;
        case '%': return modulo;
    }
}

function plus(a,b) { return a + b; } 
function minus(a,b) { return a - b; }
function multiply(a,b) { return a * b; }
function divide(a,b) { return a / b; }
function modulo(a,b) { return a % b; }
{% endhighlight %}

Finally, we'll add `console.log(postfixStack[0])`. Now you can run the script and see how it works. 

Going forward, I'd probably want to stuff all of this into its own function, and write another set of functions -- one to accept input, one to validate it, and perhaps another to parse an infix expression into a postfix.

