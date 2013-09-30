{ 
  gStyle->SetOptStat(0);  

  TFile l100("ADMXQLD111degenMG1500.0MQ1000.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_l100 = (TH1F*) l100.Get("2jet");   
  h_l100 -> SetLineColor(38); 
  h_l100 -> SetMaximum (10000); 

  h_l100 -> Draw(); 

  h_l100 -> SetTitle("M_{#tilde{g}}=1500 GeV, M_{#tilde{q}}=1000 GeV");
  h_l100 -> SetXTitle("p_{T} (l)");
  h_l100 -> SetYTitle("number of events");
  gPad -> SetLogy(); 


  Double_t firstbincontent = h_l100 -> GetBinContent (1);
  cout << "no lepton bin : " << firstbincontent << endl ;

  Double_t s = 0; 
  for( int i = 2 ; i <= 51 ; i++ ) { 
    s += h_l100->GetBinContent(i) ; 

  }

  cout << "others : " << s << endl ; 

  TArrow ar( 150,firstbincontent, 25, firstbincontent, 0.02 , "|>" ); // , 10, "|>" ) ; 
  ar.SetAngle(40); 
  ar.SetLineWidth(2);
  ar.SetLineColor(38); 
  ar.SetFillColor(38); 

  ar.Draw();

  

  TFile s100("SimplifiedSUSYMN100.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 


  Double_t firstbincontent_s100 = h_s100 -> GetBinContent (1);
  TArrow ar_s100( 150,firstbincontent_s100, 25, firstbincontent_s100, 0.02 , "|>" ); // , 10, "|>" ) ; 
  ar_s100.SetAngle(40); 
  ar_s100.SetLineWidth(2);
  ar_s100.SetLineColor(42); 
  ar_s100.SetFillColor(42); 

  ar_s100.Draw();



   
  TFile s300("SimplifiedSUSYMN300.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 


  Double_t firstbincontent_s300 = h_s300 -> GetBinContent (1);
  TArrow ar_s300( 300,firstbincontent_s300, 175, firstbincontent_s300, 0.02 , "|>" ); // , 10, "|>" ) ; 
  ar_s300.SetAngle(40); 
  ar_s300.SetLineWidth(2);
  ar_s300.SetLineColor(44); 
  ar_s300.SetFillColor(44); 

  ar_s300.Draw();


  
  TFile s500("SimplifiedSUSYMN500.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_FirstLepPT.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw("same"); 
  
  Double_t firstbincontent_s500 = h_s500 -> GetBinContent (1);
  TArrow ar_s500( 450,firstbincontent_s500, 325, firstbincontent_s500, 0.02 , "|>" ); // , 10, "|>" ) ; 
  ar_s500.SetAngle(40); 
  ar_s500.SetLineWidth(2);
  ar_s500.SetLineColor(46); 
  ar_s500.SetFillColor(46); 

  ar_s500.Draw();



  gPad->Update();
  Double_t x1 = gPad->GetX1(); 
  Double_t x2 = gPad->GetX2();
  Double_t y1 = gPad->GetY1(); 
  Double_t y2 = gPad->GetY2(); 
  Double_t xr = x2-x1; 
  Double_t yr = y2-y1; 

  // TBox b1(xr*0.7+x1,yr*0.82+y1,xr*0.72+x1,yr*0.85+y1); 
  TBox b1(xr*0.7+x1,1800,xr*0.72+x1,3500); 
  b1.SetFillColor(42); 
  b1.Draw("same"); 

  // TBox b2(xr*0.73+x1,yr*0.82+y1,xr*0.75+x1,yr*0.85+y1); 
  TBox b2(xr*0.73+x1,1800,xr*0.75+x1,3500); 
  b2.SetFillColor(44); 
  b2.Draw("same"); 

  // TBox b3(xr*0.76+x1,yr*0.82+y1,xr*0.78+x1,yr*0.85+y1); 
  TBox b3(xr*0.76+x1,1800,xr*0.78+x1,3500); 
  b3.SetFillColor(46); 
  b3.Draw("same"); 
 
  // TLatex lsim(xr*0.8+x1,yr*0.82+y1,"Sim0"); 
  TLatex lsim(xr*0.8+x1,1800,"Sim0"); 
  lsim.SetTextColor(46); 
  lsim.Draw("same"); 

  // TBox b4(xr*0.7+x1,yr*0.75+y1,xr*0.78+x1,yr*0.78+y1); 
  TBox b4(xr*0.7+x1,390,xr*0.78+x1,800); 
  b4.SetFillColor(38); 
  b4.Draw("same"); 
  

  // TLatex lqld(xr*0.8+x1,yr*0.75+y1,"QLD");
  TLatex lqld(xr*0.8+x1,390,"QLD"); 
  lqld.SetTextColor(38); 
  lqld.Draw("same"); 






  
}
