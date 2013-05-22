vp.setup(1,4)

p1 <- funkyGorilla(Drugs='yes', Alcohol='yes') + labs(title="after drug and alcohol addiction treatment")
p2 <- funkyGorilla(Drugs='yes', Alcohol='no')+ labs(title="after drug treatment")
p3 <- funkyGorilla(Drugs='no', Alcohol='yes')+ labs(title="after alcohol treatment")
p4 <- funkyGorilla(Drugs='no', Alcohol='no') + labs(title='people without any treatment')


p1