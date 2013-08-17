{ 
  TFile s100("ADMXQLD111degenMG1500.0MQ1000.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  // h_s100 -> SetLineColor(30); 
  h_s100 -> Draw(); 

  h_s100 -> SetTitle("M_{#tilde{g}}=1500 GeV, M_{#tilde{q}}=1000 GeV");
  h_s100 -> SetXTitle("p_{T} (l)");
  h_s100 -> SetYTitle("number of events");
  // TAxis *axis = h_s100->GetXaxis();
  // axis -> SetLimits(0,1000);
  gPad -> SetLogy(); 


  
}
