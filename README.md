A toy demo of the divide-and-conquer part of IDA.

A transcript of interaction:
```
What's the overall question?
> 2 + 3 + 4 + 5
Q: 2 + 3 + 4 + 5
Can you immediately answer this question? y/n
> n
What's the first subquestion then?
> 2 + 3
Any more subquestions? y/n
> y
What's the next subquestion then?
> 4 + 5
Any more subquestions? y/n
> n
Q: 2 + 3
Can you immediately answer this question? y/n
> y
Q: 4 + 5
Can you immediately answer this question? y/n
> y
What's the answer?
2 + 3
> 5
What's the answer?
4 + 5
> 9
Original question: 2 + 3 + 4 + 5
Subquestions and subanswers:
Q: 2 + 3
A: 5
Q: 4 + 5
A: 9
What's the answer?
> 14
The answer is: 14
```
