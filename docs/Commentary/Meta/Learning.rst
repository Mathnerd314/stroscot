Learning
########

Learning a language takes time and effort. Typically, programmers are pressed for time, so the goal would be to learn a "minimum viable amount" of Stroscot in a short period of time. As a project, Stroscot should provide resources to help programmers learn Stroscot efficiently and in as pleasant a manner as possible. Developers shouldn't be expected to read through the full language specification just to get started. User guides and other documentation will be provided to make it easy to learn how to use Stroscot. Both complete beginners and developers experienced with other language should be addressed.


There is clear, comprehensive, and up-to-date documentation.

  It is easy for experienced developers to understand and maintain programs.



Formats
=======

Learning content can be provided in different ways:

* Lecture (5% retention): The traditional "stand and deliver" lecture is not very effective at teaching compared to other methods, but it is cheap and easy to produce and the traditional format of teaching.

* Literature (10% retention): Books, online tutorials, and official languages references provide comprehensive and structured learning material. They often offer in-depth explanations, examples, exercises, and best practices. How long should it be? A `121 page Python book (60 pages double spaced) <https://www.amazon.com/Python-Programming-Beginners-Comprehensive-Hands/dp/B0BFV21L24/>`__ is derided as terse and useless, requiring to google every new keyword. `K&R C <https://www.amazon.com/C-Programming-Language-2nd-Edition/dp/0131103628/>`__ has 272 pages, but is "not beginner friendly". The `C# Programming Yellow Book <http://www.csharpcourse.com/>`__  is 217 8.5x11 pages or about 322 of the standard 7x9 pages. `Python for Kids <https://www.amazon.com/Python-Kids-Playful-Introduction-Programming/dp/1593274076/>`__ clocks in at 344 pages but is still missing critical functions such as the input command. On the other hand some chapters such as turtle graphics, tkinter, and classes/objects can be skipped (74 pages). My first programming book `Beginning Programming with Java For Dummies <https://www.amazon.com/Beginning-Programming-Java-Dummies-Computers/dp/0764526464/>`__ had 408 pages. The `5th edition <https://www.amazon.com/Beginning-Programming-Java-Dummies-Computer/dp/1119235537/>`__ is the most popular and has 560 pages. But it still only covers the basics. `Head First Java <https://www.amazon.com/Head-First-Java-2nd-Edition/dp/0596009208/>`__ is recommended by the r/learnprogramming subreddit and has 688 pages.

* Video Courses (20%-30% retention): Video courses offer visual and auditory learning experiences. mainly in the form of lectures and coding examples. YouTube has numerous X-hour courses on various subjects, from universities and individuals. On YouTube `MIT 6.0001 <https://ocw.mit.edu/courses/6-0001-introduction-to-computer-science-and-programming-in-python-fall-2016/video_galleries/lecture-videos/>`__ is around 12x45=540 minutes. `CS50P <https://www.youtube.com/playlist?list=PLhQjrBD2T3817j24-GogXmWqO5Q5vYy0V>`__ is 14x1.2=1005 minutes. The amateur `CS Dojo <https://www.youtube.com/playlist?list=PLBZBJbE_rGRWeh5mIBhD-hhDwSEDxogDg>`__ is 16x~13=217 minutes. `Digilent Inc.'s course <https://www.youtube.com/playlist?list=PL0845FEB57E5894C2>`__ is 87x6.5=561 minutes.

* Quizzes (? retention): Some basic tests allow quickly identifying areas where one's knowledge is deficient. Students will learn things from seeing their wrong answers and an explanation of the correct answer.

* Online platforms (75% retention): Online platforms such as edX, Udacity, and Coursera, besides offering videos, also offer interactive coding exercises, challenges, and projects. These can be highly effective for learning by doing. These platforms provide a hands-on learning experience where you can write, run, and test code directly in a web browser. They often offer immediate feedback and guidance to reinforce learning. Coursera's `Learn to program <https://www.coursera.org/learn/learn-to-program>`__ course is 291 minutes or less than 5 hours of video content but there are 43 readings and Coursera says it will take 25 hours to complete.

* Communities and Forums (50% retention): Participating in chats, communities and forums can be a valuable learning resource. Engaging with peers, mentors, and experts allows you to ask questions, seek guidance, and learn from others' experiences. It's a great way to stay updated with the latest trends and best practices, if other resources are not sufficient.

* Active learning (90%): Trying out the programming language and directly learning what works and what doesn't is the most effective method of learning. It is a bit hard to get started with the basics this way though.

* In-person instruction (5%-90% depending on activity): Although most programming languages are self-taught (SO 2019: 85.5% of respondents taught themselves a new language, framework, or tool without taking a formal course), one's first programming language is usually learned in a course. If Stroscot indeed takes over the world then it will be people's first programming language (or second, after an introductory graphical language), and they will most likely learn it via in-person instruction.

Experience
==========

Having experience with other language can make learning Stroscot faster, but other languages can also bring over preconceptions, causing "interference". For example in :cite:`joostenTeachingFunctionalProgramming1993`, imperative concepts became misconceptions in functional programming: variables can be defined after they are used, operators like ``tail``, ``take``, ``drop``, ``remove``, ``filter`` do not mutate their arguments, and there is no need to clone results to prevent them from being mutated and corrupted. `Dijkstra <https://www.cs.utexas.edu/users/EWD/ewd04xx/EWD498.PDF>`__ similarly stated that COBOL "cripples the mind" and BASIC "mentally mutilates programmers beyond hope of regeneration".

Probably Dijkstra overreaches when he states "beyond hope of regeneration" - although learning other styles of programming may introduce habits that are bad in the context of Stroscot, it is possible to unlearn habits. According to research on drug and alcohol abuse, there are several techniques:

* practice self-control by exercising a little bit of willpower every day
* identify triggers for the bad habits and work out ways to avoid those triggers
* map out positive routines and new habits to replace the old habits
* get support and track failures and successes over time

Per `this <https://www.thebioneer.com/hackers-brain-the-psychology-of-programming/>`__  the main dopamine reward for programmers is when their code compiles and works. So in order to correct bad programming habits, the compiler must withold success from the programmer, e.g. by reporting warnings on the bad style of coding and refusing to run the program.

Neuroscience
============

We can distinguish many tasks in programming:

* designing code - often happens purely in the head. Programmers take long walks or similar to figure out a problem, and then when they got home, they simply write out the code they have thought of. The "hard part" happens away from the computer.
* writing code - mechanical typing, hard to tell if it's very cognitive
* skimming code - relies on the language centres of the brain, the ventral lateral prefrontal cortex. (citation needed)
* reading code carefully for bugs
* learning a new programming language

All of these have different demands on the brain and people will be better at different aspects.

Programming misconceptions aren't likely to release dopamine,

For experienced programmers, a specific "Stroscot for Y programmers" guide series should be sufficient to retrain programmers away from their bad habits.

working memory - when you’re thinking of a sequence of events, you need to keep the line of logical reasoning held in your working memory

flow state - it’s easy to think ‘just one more compile’ and end up staying up all night.

coding provides an immediate feedback loop with testing and seeing the results. This releases Dopamine (citation needed)

TODO: check out the Unified Learning Model book

Content
=======

Core Programming Concepts: A strong understanding of fundamental programming concepts is crucial. This includes knowledge of variables, data types, control structures (loops, conditionals), functions or methods, and basic algorithms.

Syntax and Language Proficiency: Proficiency in the syntax and features of the programming language(s) required for the job is essential. This includes being comfortable with the language's syntax rules, idioms, and best practices.

Problem-Solving and Algorithmic Thinking: Programming jobs often involve problem-solving and designing efficient algorithms. The ability to analyze problems, break them down into smaller components, and develop logical solutions is highly valued.

Debugging and Troubleshooting: Proficiency in identifying and fixing code errors or bugs is important. Understanding debugging techniques and tools specific to the programming language can be valuable for resolving issues.

Data Structures and Algorithms: Familiarity with common data structures (e.g., arrays, linked lists, stacks, queues) and algorithms (e.g., sorting, searching) is typically expected. Knowing when and how to use appropriate data structures and algorithms is valuable for efficient program design.

Object-Oriented Programming (OOP): Proficiency in OOP concepts, such as classes, objects, inheritance, and polymorphism, is often required for jobs that involve OOP languages like Java, C++, or Python.

Software Development Lifecycle: Understanding the software development lifecycle, including requirements gathering, design, implementation, testing, and maintenance, is beneficial. Familiarity with version control systems, debugging tools, and software testing methodologies is often expected.

Web Development Skills (if applicable): For web development positions, proficiency in HTML, CSS, and JavaScript is typically required. Knowledge of web frameworks (e.g., React, Angular, Django) and familiarity with database systems (e.g., SQL) may also be expected.

Collaboration and Communication: Strong collaboration and communication skills are valuable in programming jobs. The ability to work well in a team, articulate ideas, and communicate effectively with colleagues or clients is often sought after.

Understanding Syntax: Familiarize yourself with the syntax and basic language constructs of the programming language. This includes learning how to declare variables, write control structures (such as loops and conditionals), define functions or methods, and work with data structures.

Proficiency in Core Concepts: Gain a solid understanding of the core concepts and principles of the programming language. This involves grasping concepts like data types, operators, control flow, object-oriented programming (if applicable), error handling, and memory management.

Reading and Understanding Code: Develop the ability to read and comprehend code written in the programming language. This includes understanding the logic, flow, and structure of programs written by others, as well as being able to navigate and interpret documentation and libraries.

Writing Functional Code: Be able to write functional and correct code in the programming language. This involves understanding the best practices, idioms, and style guidelines specific to the language. Aim to write code that is efficient, maintainable, and follows the community's coding conventions.

Problem Solving: Apply the programming language to solve problems and implement algorithms. Practice solving coding challenges, exercises, or real-life scenarios using the language's features and capabilities.

Working with Frameworks/Libraries: Explore and gain familiarity with popular frameworks, libraries, or tools associated with the programming language. Learn how to leverage these resources to build applications, websites, or perform specific tasks efficiently.

Debugging and Troubleshooting: Develop skills in identifying and fixing errors or bugs in your code. Learn how to use debugging tools and techniques specific to the programming language to diagnose and resolve issues.

Building Projects: Apply the language to build small projects or prototypes. Working on practical projects helps consolidate your learning, reinforces concepts, and provides you with tangible examples of your proficiency.

Integration and Interoperability: Understand how the programming language can integrate or interact with other technologies, such as databases, APIs, web services, or other programming languages. Learn about communication protocols, data formats, and best practices for seamless integration.

Continual Learning: Recognize that learning a programming language is an ongoing process. Stay updated with new language features, libraries, and best practices. Engage in the language's community through forums, blogs, or attending conferences to stay connected and continue expanding your knowledge.

Hands-on Practice: Active coding practice is crucial for learning a new programming language. Set aside dedicated time each day to work on coding exercises, small projects, or challenges in the new language. Experiment with the language's features, syntax, and libraries.

Work on Real-Life Examples: Apply the new language to real-life scenarios or problems similar to what you might encounter in your new job. This practical approach will strengthen your understanding and provide valuable context for using the language effectively.

Find a Mentor or Study Group: Seek out experienced programmers who are knowledgeable in the language you're learning. They can provide guidance, answer questions, and offer valuable insights. Join online communities, forums, or meetups where you can connect with like-minded learners.

Build a Project: Create a small project or contribute to an open-source project in the new language. This will give you hands-on experience and help solidify your understanding. It also demonstrates your ability to apply the language in practical scenarios.

Review and Refine: Regularly review what you've learned to reinforce your understanding. Focus on areas where you feel less confident and seek clarification on any challenging topics. Refine your code, optimize your solutions, and strive for best practices.

Seek Feedback: As you progress, seek feedback from experienced programmers or peers who are familiar with the language. They can review your code, provide suggestions, and help you improve your coding style.



if you're debugging something, you're trying to find out how to fix something, a lot of times it's because a lack of focus on details. And the linguistic part of your brain is just skipping over the details while you're coding. If you read something backwards, that turns off the linguistic parts of your brain, and then you interpret them literally, so then you spot errors, differences, deltas, things like that, they pop out at you.

typing comments above every line - That actually finds bugs because they'll type the comment above it and they think in a different mode and they go oh oh that code is wrong.

sent an email asking a question and right after they send the email they go oh I got the answer

a lot of the blockage for students to learn to code actually is a mixture of general fear, anxiety, problems with how they might have been taught how to learn when they're younger,


most programming languages really suck. Beginners make the same common errors over and over again. And with a better design, they wouldn't.
- what the hell is a dot comma (semicolon)? especially if you're in a country with a non-English keyboard.

a lot of programming is rote knowledge, not skill. Sure you have to think, but for loops - you've got to know what they are. There's some debate as to how much is rote, vs. how much is googling skill, but we'll let that pass.

If a programmer cannot inspect what a program is doing, she can't understand it.

curriculum - what topics do we teach in what order.
first one: "print hello world" or "draw a rectangle with an ellipsis that has an axis of 45 degrees across the plane that's a Cartesian plane that has X going from the Y to the top"

left frontal temporal lobe - what is its role in programming
- specifically, any implications for debugging, error reporting

for the vast majority of people this ability to switch from linguistic processing to literal visual processing is really difficult

how do you react when a student says "I don't understand"?
- blame the student - "if you can't learn it you're just stupid" - everyone will feel stupid when learning
- RTFM - So you go to read the docs. They're all over the place, horribly organized. There's no single doc that gets you started and gets going. You go, hey, the docs kind of suck. Well, read the code.
- education research model - if it's not working, it's our fault. we didn't teach it right.

evidence-based programming curriculums - do randomized experiments and see what works for teaching

maybe teachability can be used as a proxy for usability - how well people learn it and how many mistakes they made should correlate with how quickly they implement programs afterwards

because programmer are constantly learning and forgetting

it's hard to define usability, but if it's easier for to learn it and it's easy to teach it then it's probably more usable

mental models - detail is important. knowing a lot of details helps you debug. like the CPU registers and memory model and so on, when you're tracing a program in gdb you see all those. but if you're doing hello world you don't want to deal with it. so teach only a little bit at a time. don't go into the history, or the alternate designs, or anything, just give the minimum information needed to complete the task.

invalid self-perception - you don't remember how you learned something, especially if you learned it when you're a kid. you don't have metacognition when you're a kid. most adults don't even know how they know things. So you think, "oh I learned because I actually went through a book and memorized everything". But you don't remember all the other things you did before that.

Like musicians, they're like "you learn rhythm by playing with a metronome". But actually they played with a band and they had a drum and there's a lot of other things and the metronome was a pretty small part. Or guitar, he's like "you practice this with your hand", and he's got these massive fingers and you go "that's not gonna work, I can't play guita".

bottom-up versus top-down - it's actually both. people hate that.
you see this in like a lot of teaching texts, they just describe things starting at the infinite degree of detail and slowly build up to something useful
but instead could go top down and paint some broad strokes
kids you can throw a ton of details out and they'll probably pick up most of them on their own
adults you gotta go slow, like they're on a diet. a tiny bit at a time and track your progress.

one experiment: assembly language first, vs. Python first
who can implement things better at the end
how many drop out
how many retain the knowledge
how many are able to learn new programming languages

"I can reason about this therefore I am right" - or more practically, "<complicated logic>, therefore I'm right". but it's a paper with evidence - you have to have more evidence to counter it, you can't just logically disprove it. logic is not superior to empiricism. All these people had reasons why it was invalid but they were just logic, no evidence.

what is a beginner?
- a lot of people by beginner they mean junior programmer. Been through college, just showed up at work. doesn't know shit but can code.
-  I mean someone who has never opened the terminal. a "total beginner".

learn to code before you understand the concept - I was lazy, and I just did one big chunk of code instead of pieces. And I found that it was easier to walk through a big chunk of code and explain it piece-by-piece than it is to try to explain the concepts before the code and they have no idea what you're talking about. Especially, if you don't have shared vocabulary, they have no idea.

education research - go read education research journals!  in fact in most sciences and disciplines there's a branch of that discipline on just how to teach that discipline. CS is young so it doesn't have as much research in that area, but there's still tons of stuff if you look.


overly complicated obtuse error messages

hack-fix cycle - the code builds up a history, all these minutia of fixes and hacks. then six months later you can't work with the code because you forgot those things and it looks like spaghetti. Whereas, you pull back and do up-front thinking, you come up with the actual theme and a clean design. Write code so that someone can come in with no history of its creation and understand it immediately.

pushing bad ideas - they made Python 3. Nobody switched. Why? They didn't bother finding out, they just kept pushing Python 3.

pride - they go, well, that's to reduce the number of functions you have to remember. What? it's two fucking functions to convert a string to a timestamp, and one of them in a different. They back into their justification to cover up a design flaw, and then blame you when you don't agree with that justification. But it's just kind of covering pride. Inconsistent APIs due to a lack of a global view.

What kind of programming language would I recommend to beginners, eight-year-old? it sort of doesn't matter, as long as it's one that doesn't have a lot of difficulty to handle crashes and things like that. So Python would be a good start. The reason why it doesn't matter is, first off, all programming languages kind of aren't targeted for beginners. They suck. And second, the language itself is not the hard part for a beginner. It's the computational concepts, like looping, hierarchy, variables, scope. So any language kind of teaches that. Plain old basic with go-tos would also be a really good one. That's a good beginner language. OK, feel free to come up and talk to me and tell me I'm full of shit. Thank you very much. Thank you.

From :cite:`jonesEstimatingSoftwareCosts2007`: "Mnemonic complexity appears to be a factor in learning and using programming languages"

ergonomics - a measure of the friction you experience when trying to get things done with a tool
productivity
learning curve
streamlining features
glossing over irrelevant details.
"flow" - ideas and intuitions are steadily transformed into working, reliable, fast code with a minimum of fuss
interruptions - looking things up, switching contexts, going through a large amount of ceremony, or dealing with reams of errors where the compiler pedantically points out tiny things you forgot. Anything that takes your attention away from the problem at hand and puts it on details that don't really matter (or don't matter just yet).
momentum - how easy it is to remember the right thing to type

