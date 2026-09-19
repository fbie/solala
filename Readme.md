# Social Law Language #

(In German "so la-la" means roughly "so-so" or "nogenlunde".)

A tiny DSL for expressing Danish social law (sociallovgivning) formally with the goal to automatically generate a workflow that allows for quick and dirty eligibility checking.

## Why ##

It is really difficult to figure out, which benefits you are eligible for, if you live with a permanent handicap. Municipalities are required to inform citizens of their rights and which benefits they could apply for ("oplysningspligt"), but in reality that does happen rarely to an exhaustive degree.

What if instead, the social benefits law was discoverable through a simple user interface?

- What do you want to apply for?
- Which category of benefits, personal help, monetary compensation?
- Here are your options, would you like to know for which you are eligible?
- Enter some data, the same way you would provide it when applying for a benefit.

Finally, you receive a short report that explains whether you are eligible, which conditions you may not fulfill if you are not, and which parts of the decision ultimately reside with the municipality.

## What ##

Social law is often structured in a way that resembles a list of conditions: given these conditions hold, an applicant (or on whose behalf an applicant applies) is eligible for some benefit.

The conditions rarely involve complicated computations. Instead, they assert facts (e.g. is the applicant of age, is a handicap permanent, is some service necessary in relation to a certain physical condition), exhaustiveness checks (e.g. does the applicant already receive another benefit that is mutually exclusive with the benefit in question) or defer decisions to the discretion of a case worker.

The core idea is hence to take the formalized law text and, for each reference to the applicant, ask the applicant for data (unless the data is already known.) That enables a straightforward translation of law text into formalized expressions without the need to code a user interface that facilitates the interaction.

## References ##

This is of course not a novel idea. Generating workflows from formal specifications has been done many times before. I have not performed a thorough analysis of the literature, but here are a few properly implemented instances of the idea, just better than what resides in this repository:

- [Catala](https://catala-lang.org/), a domain-specific language for the law; and
- [DCR graphs](https://dcrsolutions.net/), a flexible way to model workflows.
