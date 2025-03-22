# Individual assignment

In this assignment,
you will work on your own to
build a less complex app that in your group project.
The purpose of this assignment is for you to learn more about dashboard libraries
and get familiar with the general concepts of building dashboards
versus the specific implementation in the package you chose to work with in your group project.

## 1. Submission instructions and repo creation
rubric={mechanics:5}

- [Follow the general lab instructions](https://ubc-mds.github.io/resources_pages/general_lab_instructions/).
- [Click here to view a description of the rubrics used to grade the questions](https://github.com/UBC-MDS/public/tree/master/rubric).
- Develop your app in your individual repo on UBC's GitHub (so the repo in which you received these instructions).
- At the end of the project, create a release with the final version of the code before the submission deadline. You don't need to include release notes.
- Since you are working on this project on your own, you are not required to work with branches, PR, and issues. It can still be a good idea to do so, to effectivel ytrack your ideas and their implementation.
- You need to **submit one file on Gradescope:**
  - `link-to-release.ipynb`: Includes a single markdown cell with the link to your release on GitHub.ubc.ca.

## 2. Repository structure
rubric={accuracy:10}

Since this is a private repo,
you only need to include the following:

1. `LICENSE.md`
2. `src/app.py` (or the equivalent for whatever dashboard library you pick)
3. `data/raw/` (should include your chosen raw data file(s); indicate in the readme if you load from an external source instead)
4. `environment.yml` (or the equivalent if you work with another environment manager; should contain everything that is required to run your dashboard in case we need to)
5. `img/demo.mp4` a short video of you walking through your app (see below for details)
6. `README.md` (see below)

## 3. README
rubric={reasoning:10}

The README should include a short motivation, app description, and installation instructions.

### Motivation

Write a short motivation (max 150 words) about who your target audience is
and what you aim for them to achieve using the app.
This can be much briefer than the group assignment.

Example:

> Target audience: Health care administrators
>
> Missed medical appointments cost the healthcare system a lot of money
> and affects the quality of care.
> If we could understand which factors lead to missed appointments
> it may be possible to reduce their frequency
> and use the saved resources to improve patient outcomes.
> To address this challenge,
> this data visualization app allows health care administrators
> to visually explore a dataset of missed appointments to identify potentially underlying factors,
> such as the day of the week, the age of the patient,
> and the time between scheduling and the appointment.
> The app shows the distribution of factors contributing to appointment show/no show
> and allows users to explore different aspects of this data
> by filtering and re-ordering on different variables
> in order to compare factors that contribute to absence.

### App description

Instead of writing down a description by hand,
include a short video here of you describing the app
(see the next section for details).
Either linking to the file or showing the video directly in the readme is fine
(e.g. by uploading to another hosting service or GitHub directly by editing the file in the online interface).

### Installation instructions

Detail how to install the app and how to run it locally.

## 4. Video walkthrough
rubric={reasoning:15}

Since we don't require you to deploy these apps,
we ask you to create a short (~5 min) video  walkthrough of how to use it.
The video should start with you briefly mentioning the purpose and target audience of the app
and then proceed to demonstrate it's full functionality.

You can use your screen recording software,
and don't need to worry about anything fancy
like your portrait being in the video.
Just make sure that we can clearly see what you are clicking in the dashboard
and hear what you are saying.
Avoid creating a huge file if you can,
~10 MB is reasonable for a 5-minute video.
You can use [Handbrake](https://handbrake.fr/) to compress the video
if it is much bigger than that
but we won't grade this part.

Here is a video I made so that you can see what I am looking for:

https://media.github.ubc.ca/user/1751/files/f5d98efd-abfd-43a2-8613-2ca5e0a260eb

## 5. App design and functionality
rubric={accuracy:10,reasoning:20,viz:25}

Your app should meet the following specification:

- Minimum four inputs/outputs and at least one of each (e.g. three widgets and one chart).
- Cannot use the same library as for the group app or replicate the same dashboard layout/functionality
  - It's fine to use the same data and create a different dashboard
- You cannot create the same dashboard as someone else
- If you want to use another library than Dash/Shiny,
  you need to DM me on Slack to have it approved first.
    - For example, if you are really excited about learning Streamlit,
      or want to explore Tableau/Power BI,
      this is your chance to do that while receiving some credit for it.
    - The lecture notes/slides mention several other frameworks you could use
      and there is a link to a Tableau course from the past.
    - The lecture notes will cover how to do most things you need in Shiny R,
      and link resources where we don't cover it directly.
      If you pick another framework, it is up to you to find the material you need.
    - The idea is that you should learn something new,
      so don't pick a dashboard framework you know well from before.
- You should follow the best practices for dashboards that we have talked about in 532
  (and for viz from 531),
  or clearly explain why you didn't.
    - E.g. Lay out dashboard elements in a grid, include a title, organize widgets logically, etc.

## 6. Add an advanced aspect to your app (challenging)
rubric={reasoning:5}

To attempt this part of the assignment,
you would add something that goes beyond the specification I listed above.
This could be one of several things:

1. Deploy your dashboard publicly on the web.
    - Depending on which library you have chosen to work with,
      this procedure will differ
      (and if you have chosen something esoteric,
      it might not even support deployment).
    - It's OK if you need to copy your code into a public git repo
      (and you might want to do that anyways to make this app part of your portfolio)
      or if you deploy without linking the deployment to your repo (e.g. from a zip),
      but you should include a link to the public deployment in "Website" field
      of the "About" section in your UBC GitHub repo.
2. Add additional inputs/outputs.
    - The minimum requirement is six inputs/outputs in total.
3. Add advanced callback functionality.
    - E.g. Maybe you want to perform a computation in Python upon a selection
      in a chart that updates some other part of your dashboard.
    - Please reach out to me if you have an idea here that you are unsure if it will count.
4. Reach out to me if you have another idea of what you consider advanced
    - I want you to have freedom in what you choose to create,
      but also make sure that we are on the same page
      regarding whether it will count towards the challenging part of this question
      before you spend time on it.

You **need to mention if you attempted the challenging question in the video**
so that the TA knows what to asses for it.
E.g. you describe the callback or the additional widgets
that you added as part of the challenging question.
