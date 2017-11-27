
   
                                     #### ASSOCIATION RULES MINING ######
									  
>library("arulesViz")
>library("arules")									  
>bitmat <- as.matrix(bit_dataset_cleaned_filtered)
> trans<-as(bitmat,"transactions") # Converting data into Transactions 
> rules <- apriori(trans)  # Creating rules based on the Apriori Package
Apriori
          # Output Obtained
Parameter specification:
 confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target
        0.8    0.1    1 none FALSE            TRUE       5     0.1      1     10  rules
   ext
 FALSE
        # Output Obtained
Algorithmic control:
 filter tree heap memopt load sort verbose
    0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 292 # Support Count Obtained

set item appearances ...[0 item(s)] done [0.00s].
set transactions ...[17 item(s), 2920 transaction(s)] done [0.00s].
sorting and recoding items ... [17 item(s)] done [0.00s].
creating transaction tree ... done [0.00s].
checking subsets of size 1 2 3 4 5 6 7 8 9 10 done [0.08s].
writing ... [860931 rule(s)] done [0.40s].
creating S4 object  ... done [0.57s].
Warning message:
In apriori(trans) :
  Mining stopped (maxlen reached). Only patterns up to a length of 10 returned!
  
> inspect(head(sort(rules, by="lift"),3)); # 860931 rule is a huge number. So we’re going to trim down the rules to the ones that are more important TOP 3.
   
   # Output Obtained 

   lhs    rhs                                support   confidence lift count
[1] {}  => {btc_market_price_label}           0.8794521 0.8794521  1    2568 
[2] {}  => {btc_transaction_to_trade_ratio}   1.0000000 1.0000000  1    2920 
[3] {}  => {btc_estimated_transaction_volume} 1.0000000 1.0000000  1    2920 
  
> library(arulesViz) # Visualizing the rules 
  
> plot(rules, shading="order", control=list(main ="Two-key plot")); # Two key Plot
  
> plot(rules[quality(rules)$support > 0.99], method="graph", control=list(type="items")) #graph for 100 rules
  
  
> sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);  #Plotting rules based on Support , Lift , Confidence
  
> subrules = rules[quality(rules)$confidence > 0.99]; #Creating SubRules having confidence greater 0.99
  
> plot(subrules, method="matrix", measure="lift"); #Plotting subrules created above  with Consequent and Antecedent.
  
> subrules2 = head(sort(rules, by="lift"), 30);  # Creating subset of  30 rules based on Lift

> plot(subrules2, method="graph"); # Graph for 30 rules

> oneRule = sample(rules, 10); #  Creating top 10 rules.

> inspect(oneRule);

                 # Output Obtained
    # lhs                                   rhs                               support   confidence   lift    count
[1]  {btc_total_bitcoins,                                                                                  
      btc_market_cap,                                                                                      
      btc_trade_volume,                                                                                    
      btc_n_orphaned_blocks,                                                                               
      btc_hash_rate,                                                                                       
      btc_miners_revenue,                                                                                  
      btc_transaction_fees,                                                                                
      btc_transaction_to_trade_ratio,                                                                      
      btc_market_price_label}           => {btc_avg_block_size}             0.8794521          1       1     2568
[2]  {btc_total_bitcoins,                                                                                  
      btc_avg_block_size,                                                                                  
      btc_n_orphaned_blocks,                                                                               
      btc_difficulty,                                                                                      
      btc_miners_revenue,                                                                                  
      btc_transaction_fees,                                                                                
      btc_cost_per_transaction_percent,                                                                    
      btc_estimated_transaction_volume,                                                                    
      btc_market_price_label}           => {btc_n_transactions}             0.8794521          1       1    2568
[3]  {btc_total_bitcoins,                                                                                  
      btc_trade_volume,                                                                                    
      btc_avg_block_size,                                                                                  
      btc_n_transactions_per_block,                                                                        
      btc_hash_rate,                                                                                       
      btc_n_unique_addresses,                                                                              
      btc_estimated_transaction_volume} => {btc_market_cap}                 1.0000000          1        1   2920
[4]  {btc_trade_volume,                                                                                    
      btc_avg_block_size,                                                                                  
      btc_n_transactions_per_block,                                                                        
      btc_miners_revenue,                                                                                  
      btc_cost_per_transaction_percent,                                                                    
      btc_n_unique_addresses,                                                                              
      btc_n_transactions,                                                                                  
      btc_market_price_label}           => {btc_total_bitcoins}             0.8794521          1        1   2568
[5]  {btc_total_bitcoins,                                                                                  
      btc_n_orphaned_blocks,                                                                               
      btc_n_transactions_per_block,                                                                        
      btc_median_confirmation_time,                                                                        
      btc_miners_revenue,                                                                                  
      btc_transaction_fees,                                                                                
      btc_cost_per_transaction_percent,                                                                    
      btc_n_transactions}               => {btc_n_unique_addresses}         1.0000000          1        1   2920
[6]  {btc_total_bitcoins,                                                                                  
      btc_market_cap,                                                                                      
      btc_n_orphaned_blocks,                                                                               
      btc_hash_rate,                                                                                       
      btc_difficulty,                                                                                      
      btc_miners_revenue,                                                                                  
      btc_transaction_fees,                                                                                
      btc_estimated_transaction_volume} => {btc_transaction_to_trade_ratio} 1.0000000          1       1    2920
[7]  {btc_n_orphaned_blocks,                                                                               
      btc_median_confirmation_time,                                                                        
      btc_hash_rate,                                                                                       
      btc_miners_revenue,                                                                                  
      btc_transaction_fees,                                                                                
      btc_cost_per_transaction_percent,                                                                    
      btc_market_price_label}           => {btc_avg_block_size}             0.8794521          1       1    2568
[8]  {btc_avg_block_size,                                                                                  
      btc_median_confirmation_time,                                                                        
      btc_hash_rate,                                                                                       
      btc_miners_revenue,                                                                                  
      btc_n_unique_addresses,                                                                              
      btc_n_transactions,                                                                                  
      btc_transaction_to_trade_ratio}   => {btc_trade_volume}               1.0000000          1       1    2920
[9]  {btc_avg_block_size,                                                                                  
      btc_median_confirmation_time,                                                                        
      btc_hash_rate,                                                                                       
      btc_miners_revenue,                                                                                  
      btc_transaction_fees,                                                                                
      btc_cost_per_transaction_percent,                                                                    
      btc_n_unique_addresses,                                                                              
      btc_n_transactions,                                                                                  
      btc_estimated_transaction_volume} => {btc_market_cap}                 1.0000000          1       1   2920
[10] {btc_total_bitcoins,                                                                                  
      btc_market_cap,                                                                                      
      btc_n_transactions_per_block,                                                                        
      btc_difficulty,                                                                                      
      btc_miners_revenue,                                                                                  
      btc_cost_per_transaction_percent,                                                                    
      btc_transaction_to_trade_ratio,                                                                      
      btc_market_price_label}           => {btc_transaction_fees}           0.8794521          1       1   2568
  
  
  
  
  
  