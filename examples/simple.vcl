pi = 3.14159

@parameter
user_pi: Real

is_pi_pi: Real -> Bool
is_pi_pi x = pi == user_pi

@property
pi_equals_itself: Bool
pi_equals_itself = is_pi_pi user_pi