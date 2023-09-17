![image](https://github.com/robertlisaru/elm-exercises/assets/40792547/9a2e1778-1e23-4ac0-8689-c0470aad64b8)

## Description
My solutions to johncrane's [Ninety-nine Problems, Solved in Elm](https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/).

## To do
- [x] setup CI
- [x] setup elm-review
- [x] refactor into proper unit tests
- [ ] refactor tests to be more granular
- [ ] allow input of a custom list to run the problems on
- [ ] nicer ui
- [ ] setup dev server



## About Elm
[Elm](https://elm-lang.org/) is a purely functional programming language and a frontend web framework. If you are not used to functional programming, it will be challenging at first. But once you learn and use it for a while, it becomes a pleasure to write code. 
- Most of the bugs reveal themselves at `compile time`.
- No hidden `runtime exceptions` will be breaking your production code.
- Functions have no `side effects`.
- Application `components` are decopled and don't break eachother.
- If you want to make a change, you just do it without the fear of breaking something else, as the `compiler` will guide you through all the code that needs to be updated to accomodate your change without breaking anything else.
- `Error messages` are informative and you immediately know what to do.
- No more `undefined` runtime errors. Everything is always defined.
- `Application flow` is easy to follow in code.
- Elm developers are friendly, helpful, and always excited to talk about Elm.

## My Elm experience <3
- After React, this was the 2nd web framework I had ever interacted with at the time, in 2021.
- At university I had mostly used imperative languages like Java, C, C++, PHP.
- Thinking in a functional style was not easy at first, but shortly became natural and intuitive to me.
- It took me 7 days to learn the Elm basics and build a [small test app](https://github.com/robertlisaru/elm-todos-app).
- After that, I jumped straight into a real-world production project, where I successfully deployed over 100 pull requests, and many thousand lines of code.

The elm tree is also a wonderful tree.

![image](https://github.com/robertlisaru/elm-exercises/assets/40792547/34d16f6f-3f13-4c66-b845-8a57cf5d4fd8)

## Initial setup
Install dependencies:
```shell
npm install
```

## Starting live server
Run the start script:

```shell
npm start
```
It will open the app in the browser and watch for code changes.

## Running unit tests
Run the test script:
```shell
npm run test
```
It will run everything in the `tests` folder.

## Before commiting
Run the review script:
```shell
npm run review
```
It will review the code using the rules from `ReviewConfig.elm` file.