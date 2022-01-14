# did2s 0.5.0

- Made code more robust by improving sparse model matrix creation

- Rewrote covariance matrix creation to be faster and more robust

- Improved block bootstrapping code using `boot::boot`

- Fixed Sun and Abraham bug in `event_study`, h/t Frederik Noack
	

# did2s 0.4.0

- Add `event_study` and `plot_event_study` functions

- Seperated `did_imputation` to new package `didimputation`

# did2s 0.3.0

- Add `did_imputation` following Borusyak, Jaravel, and Spiess (2021)

- Speed up did2s

# did2s 0.2.0

- Use sparse matrices to use much less memory

- Fix analytic standard errors when using weights

# did2s 0.1.0

- Correct and robust standard errors
