# lmrs: The libmastodon Registration Service

libmastodon applications (the ones I intend to write anyway) will not run in
environments where the user necessarily has easy access to a local web browser.
This makes doing the oauth dance to log in challenging. This service exists to
facilitate that.

When a libmastodon consumer wishes to authenticate against an instance, the
following happens:

- it will register with the lmrs
- produce a short URL to the lmrs and present it to the user
- the user will go to this URL, and lmrs will redirect them with the appropriate
  settings to the instance's login page
- once authentication is complete the user is redirected back to the lmrs
- the lmrs receives the oauth code from the instance, and instructs the user to
  return to their application
- the application fetches the oauth code from the lmrs, and uses it to receive
  an authentication token and a refresh code from the instance

_Note: the word "instance" here is used to refer to the Mastodon instance
specifically_

The lmrs also provides a client library for applications to consume, to make it
easy to interact with.
