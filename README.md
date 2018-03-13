# maxima-packages

PREFACE

maxima-packages is an experimental system for developing and distributing
packages for Maxima. At this time (March 2018), there are still some issues to be
resolved, and it is possible that maxima-packages might change very substantially
or even might be discontinued in favor of some other system.

OVERVIEW

maxima-packages is a collection of packages for the Maxima computer algebra system
which have been contributed by users.

These packages have not been reviewed carefully,
neither for correctness nor for security problems.
Users accept all risk for using these packages.

All packages in maxima-packages can be modified by others,
via the same mechanism (a pull request) by which the package was originally created.
Project administrators will give the original package author an opportunity
to review pull requests for their packages,
but if the original author does not take action to accept or reject the pull request,
project administrators will take action, at their discretion.

At this time (February 2018), there is not yet an automatic mechanism
to download and install these packages, but such a mechanism is under development.

CONTRIBUTION POLICY

 * Project administrators will accept almost any contribution made in good faith,
   although they reserve the right to reject any contribution.
 * The following are encouraged, but not required:
  - Contributors are encouraged to contribute under their real name, not a pseudonym
  - Contributors are encouraged to write documentation, preferably in Texinfo format
   * Contributors can adapt MYPACKAGE.texi for their own package
  - Contributors are encouraged to include a test script
  - Contributors are encouraged to keep the size of packages relatively small

CONTRIBUTING TO MAXIMA-PACKAGES

 1. The contributor must have a Github account.
 2. The contributor creates a branch of the maxima-packages repository.
 3. The contributor copies their branch to a local working copy.
 4. The contributor creates new files and folder, or makes any other changes in their working copy.
 5. The contributor commits their changes and pushes them to their branch of maxima-packages.
 6. The contributor issues a pull request to the maxima-packages project administrators.
 7. Project administrators review the pull request and accept or reject it,
   according to the criteria discussed above.

In more detail:

 1. The contributor must have a Github account.
  * Anyone can create a free Github account.
 2. The contributor creates a branch of the maxima-packages repository.
  * After signing in to Github, navigate to the maxima-packages page.
  * Click the "Fork" button at upper right.
 3. The contributor copies their branch to a local working copy.
  * Navigate to the branch of maxima-packages created by "Fork".
  * Click the "Clone or download" button
  * Copy the content of the text box "git clone ..."
  * Paste "git clone ..." to a command line and execute it
 4. The contributor creates new files and folder, or makes any other changes in their working copy.
  * Create a top-level folder with the same name as your Github user name
  * Create a folder under the user name folder for each package you create
  * Create files and folders under the package folder as appropriate
  * Use whatever text editor or other means to create and modify files
 5. The contributor commits their changes and pushes them to their branch of maxima-packages.
  * Execute git commands such as "git add" and "git commit"
  * An explanation of Git is beyond the scope of this document.
    You will find one tutorial introduction at: https://git-scm.com/docs/gittutorial
    There is a great deal of other information about Git, which a web search will find.
 6. The contributor issues a pull request to the maxima-packages project administrators.
  * Navigate to your branch of maxima-packages at Github
  * Click the "Pull Request" button
 7. Project administrators review the pull request and accept or reject it,
   according to the criteria discussed above.
  * If your pull request is accepted, delete your fork of maxima-packages
    (you will create a new fork each time you want to make a pull request)
