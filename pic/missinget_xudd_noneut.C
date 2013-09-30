{ 
  gStyle->SetOptStat(0);  

  
  TFile l("ADMXUDD112degenMG2500.0MQ1500.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root");
  TH1F* h_l = (TH1F*) l.Get("2jet"); 
  h_l -> SetLineColor(38); 
  h_l -> SetTitle("M_{#tilde{g}}=2500 GeV, M_{#tilde{q}}=1500 GeV");
  h_l -> SetXTitle("E_{T}^{miss}");
  h_l -> SetYTitle("Number of Events");  
  h_l -> Draw(); 


  

  TFile s500("SimplifiedSUSYMN500.0MG2500.0MSQ1500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw("same"); 

  TFile s100("SimplifiedSUSYMN100.0MG2500.0MSQ1500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 



  TLatex l1(150,1,"XUDD");
  l1.SetTextColor(38);  
  TLatex l2(600,1,"m_{N}=100"); 
  l2.SetTextColor(42);
  l1.Draw("same"); 
  l2.Draw("same"); 
  
  

 
  TFile s300("SimplifiedSUSYMN300.0MG2500.0MSQ1500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l4(550,1,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l4.Draw("same"); 



 

  TLatex l6(500,1,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l6.Draw("same"); 


  gPad->Update();
  Double_t x1 = gPad->GetX1(); 
  Double_t x2 = gPad->GetX2();
  Double_t y1 = gPad->GetY1(); 
  Double_t y2 = gPad->GetY2(); 
  Double_t xr = x2-x1; 
  Double_t yr = y2-y1; 

  TBox b1(xr*0.7+x1,yr*0.82+y1,xr*0.72+x1,yr*0.85+y1); 
  b1.SetFillColor(42); 
  b1.Draw("same"); 

  TBox b2(xr*0.73+x1,yr*0.82+y1,xr*0.75+x1,yr*0.85+y1); 
  b2.SetFillColor(44); 
  b2.Draw("same"); 

  TBox b3(xr*0.76+x1,yr*0.82+y1,xr*0.78+x1,yr*0.85+y1); 
  b3.SetFillColor(46); 
  b3.Draw("same"); 
 
  TLatex lsim(xr*0.8+x1,yr*0.82+y1,"Sim0"); 
  lsim.SetTextColor(46); 
  lsim.Draw("same"); 

  TBox b4(xr*0.7+x1,yr*0.75+y1,xr*0.78+x1,yr*0.78+y1); 
  b4.SetFillColor(38); 
  b4.Draw("same"); 
  

  TLatex lqld(xr*0.8+x1,yr*0.75+y1,"UDD"); 
  lqld.SetTextColor(38); 
  lqld.Draw("same"); 

   
  

}
