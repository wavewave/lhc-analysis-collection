{ 
  TFile l100("ADMXQLD111degenMG1500.0MQ1000.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_l100 = (TH1F*) l100.Get("2jet");   
  h_l100 -> SetLineColor(30); 
  h_l100 -> Draw(); 

  h_l100 -> SetTitle("M_{#tilde{g}}=1500 GeV, M_{#tilde{q}}=1000 GeV");
  h_l100 -> SetXTitle("p_{T} (l)");
  h_l100 -> SetYTitle("number of events");
  gPad -> SetLogy(); 

  
  TFile s100("SimplifiedSUSYMN100.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 

   
  TFile s300("SimplifiedSUSYMN300.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  
  TFile s500("SimplifiedSUSYMN500.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw("same"); 
  




  
}
